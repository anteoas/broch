(ns units.impl
  (:require [clojure.math :as math]
            [clojure.string :as string]
            [units.protocols :as prot])
  (:import (clojure.lang Ratio)))

(defn scale [n m] (when (and n m) (* n m)))

(defn to-base-number [u] (scale (prot/->number u) (prot/->scale-of-base u)))
(defn from-base-number [u n] (prot/with-num u (scale n (/ 1 (prot/->scale-of-base u)))))

(defn same-measure? [x y]
  (cond
    (and (satisfies? prot/IUnit x)
         (satisfies? prot/IUnit y)) (= (prot/->measure x) (prot/->measure y))
    :else false))

(defn convert [a b]
  (from-base-number b (to-base-number a)))

(defn print-unit [u]
  (str "#" (name (prot/->measure u)) "/" (name (prot/->symb u)) "\"" (prot/->number u) "\""))

(defn- parse-ratio [^String s]
  (let [svec (string/split s #"/")]
    (when (and (= 2 (count svec)) (every? parse-long svec))
      (let [[numerator denominator] svec]
        (new Ratio (biginteger numerator) (biginteger denominator))))))

(defn parse-number [^String s]
  (or (parse-long s) (parse-ratio s) (parse-double s) (biginteger s)))

(defn- hash-unit [u]
  (if (prot/->number u)
    (hash {:measure (prot/->measure u) :number (to-base-number (prot/->number u))})
    (hash {:measure (prot/->measure u) :symb (prot/->symb u)})))

(defn- equal-units? [u v]
  (and (= (class u) (class v)) (same-measure? u v)
       (or (and (every? (comp nil? prot/->number) [u v])
                (= (prot/->symb u) (prot/->symb v)))
           (and (every? (comp some? prot/->number) [u v])
                (== (to-base-number u) (to-base-number v))))))

;; Basic Unit

(deftype Unit [measure symb scale-of-base number]
  Object
  (toString [this] (print-unit this))
  (hashCode [this] (hash-unit this))
  (equals [this other] (equal-units? this other))

  Comparable
  (compareTo [this other] (if (same-measure? this other)
                            (- (to-base-number this) (to-base-number other))
                            (throw (ex-info "Cannot compare units of different measure." {:x this :y other}))))

  prot/IUnit
  (->measure [_] measure)
  (->number [_] number)
  (->symb [_] symb)
  (->scale-of-base [_] scale-of-base)
  (with-num [_ n] (new Unit measure symb scale-of-base n)))


;; Derived Unit

(defn- unit-symbol [unit ^Integer exponent]
  (let [symb (name (prot/->symb unit))
        exp (Math/abs exponent)]
    (if (= 1 exp)
      symb
      (str symb exp))))

(defn- d-symbol-part [unit-seq]
  (->> (sort-by (comp prot/->symb first) unit-seq)
       (map (partial apply unit-symbol))
       (string/join "-")))

(defn- derived-symbol [units]
  (let [numerators (filter (comp pos? second) units)
        denominators (filter (comp neg? second) units)]
    (if (empty? denominators)
      (d-symbol-part numerators)
      (->> (map d-symbol-part [numerators denominators])
           (string/join ":")))))

(defn- partial-scale [unit exponential]
  (math/pow (prot/->scale-of-base unit) exponential))

(deftype Derived [measure units symb number]
  Object
  (toString [this] (print-unit this))
  (hashCode [this] (hash-unit this))
  (equals [this other] (equal-units? this other))

  Comparable
  (compareTo [this other] (if (same-measure? this other)
                            (- (to-base-number this) (to-base-number other))
                            (throw (ex-info "Cannot compare units of different measure." {:x this :y other}))))

  prot/IUnit
  (->measure [_] measure)
  (->number [_] number)
  (->symb [_] (if symb symb (derived-symbol units)))
  (->scale-of-base [_] (->> units
                            (map (partial apply partial-scale))
                            (apply *)))
  (with-num [_ n] (new Derived measure units symb n)))

(defonce ^:private registry (atom {}))

(defn register-unit! [unit]
  (swap! registry assoc {(prot/with-num unit nil) 1} (prot/with-num unit nil)))
(defn register-derived! [derived]
  (swap! registry assoc (.units derived) derived))

(defn- units [x]
  (cond
    (instance? Unit x) {(prot/with-num x nil) 1}
    (instance? Derived x) (.units x)))

(defn- derive-units [x y op]
  (let [units-x (units x)
        units-y (if (= / op)
                  (update-vals (units y) -)
                  (units y))]
    (->> (merge-with + units-x units-y)
         (filter (comp not zero? second))
         (into {}))))

(defn attempt-derivation [x y op]
  (let [derived (derive-units x y op)
        num-x (prot/->number x)
        num-y (prot/->number y)]
    (cond
      (empty? derived) (op num-x num-y)

      (@registry derived) (prot/with-num (@registry derived) (op num-x num-y))

      :else (throw (ex-info (str "No derived unit is registered for " (derived-symbol derived))
                            derived)))))





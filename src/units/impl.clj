(ns units.impl
  (:require [units.protocols :as prot]
            [clojure.string :as string]
            [clojure.math :as math])
  (:import (clojure.lang Keyword)))

(defn scale [n m] (when (and n m) (* n m)))

(defn to-base-number [u] (scale (prot/->number u) (prot/->scale-of-base u)))
(defn from-base-number [u n] (prot/with-num u (scale n (/ 1 (prot/->scale-of-base u)))))

(defn same-measure? [x y]
  (cond
    (and (satisfies? prot/IUnit x)
         (satisfies? prot/IUnit y)) (= (prot/->measure x) (prot/->measure y))
    :else false))

(defn ex-incompatible-measures! [^Keyword m-x ^Keyword m-y]
  (ex-info (str "Cannot convert units.") {:from m-x :to m-y}))
(defn ex-incompatible-units! [x y]
  (ex-incompatible-measures! (prot/->measure x) (prot/->measure y)))

(defn convert [a b]
  (from-base-number b (to-base-number a)))


;; Basic Unit

(deftype Unit [measure symb scale-of-base number]
  Object
  (toString [_] (str number))
  (hashCode [this] (hash {:measure measure :number (to-base-number this)}))
  (equals [this other]
    (and (instance? Unit other) (same-measure? this other)
         (or (every? (comp nil? prot/->number) [this other])
             (and (every? (comp some? prot/->number) [this other])
                  (== (to-base-number this) (to-base-number other))))))

  Comparable
  (compareTo [this other] (if (same-measure? this other)
                            (- (to-base-number this) (to-base-number other))
                            (throw (ex-incompatible-units! this other))))

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
      (str symb "^" exp))))

(defn- d-symbol-part [unit-seq]
  (->> (sort-by first unit-seq)
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
  (toString [_] (str number))
  (hashCode [this] (hash {:measure measure :number (to-base-number this)}))
  (equals [this other]
    (and (instance? Derived other) (same-measure? this other)
         (or (every? (comp nil? prot/->number) [this other])
             (and (every? (comp some? prot/->number) [this other])
                  (== (to-base-number this) (to-base-number other))))))

  Comparable
  (compareTo [this other] (if (same-measure? this other)
                            (- (to-base-number this) (to-base-number other))
                            (throw (ex-incompatible-units! this other))))

  prot/IUnit
  (->measure [_] measure)
  (->number [_] number)
  (->symb [_] (if symb symb (derived-symbol units)))
  (->scale-of-base [_] (->> units
                            (map (partial apply partial-scale))
                            (apply *)))
  (with-num [_ n] (new Derived measure units symb n)))

(def registry (atom {}))

(defn register-derived! [derived]
  (swap! registry assoc (.units derived) derived))

(defn units [x]
  (cond
    (instance? Unit x) {(prot/with-num x nil) 1}
    (instance? Derived x) (.units x)))

(defn derive-units [x y op]
  (let [units-x (units x)
        units-y (if (= / op)
                  (update-vals (units y) -)
                  (units y))]
    (merge-with + units-x units-y)))

(defn existing-derived [x y op]
  (@registry (derive-units x y op)))





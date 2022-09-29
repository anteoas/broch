(ns units.impl
  (:require [clojure.string :as string]
            [units.protocols :as prot]))

(defn- scale [n m] (when (and n m) (* n m)))

(defn unit? [x] (satisfies? prot/IUnit x))

(defn same-measure? [x y]
  (cond
    (and (unit? x) (unit? y)) (= (prot/->measure x) (prot/->measure y))

    :else false))

(defn- convert [a b]
  (prot/from-base-number b (prot/to-base-number a)))

(defn new-unit [new-u x]
  (cond
    (nil? x)
    new-u

    (number? x)
    (prot/with-num new-u x)

    (string? x)
    (let [n (read-string x)]
      (if (number? n)
        (prot/with-num new-u n)
        (throw (ex-info (str "Can't make units with other things than numbers: " x) {:x x}))))

    (and (unit? x) (same-measure? x new-u))
    (convert x new-u)

    (and (unit? x) (not (same-measure? x new-u)))
    (throw (ex-info (str "Cannot convert a " x " into " new-u) {:fomr x :to new-u}))

    :else (throw (ex-info "Unhandled case." {:unit new-u :x x}))))

(defn- hash-unit [u]
  (if (prot/->number u)
    (hash {:measure (prot/->measure u) :number (prot/to-base-number u)})
    (hash {:measure (prot/->measure u) :symb (prot/->symb u)})))

(defn- equal-units? [u v]
  (and (= (class u) (class v)) (same-measure? u v)
       (or (and (every? (comp nil? prot/->number) [u v])
                (= (prot/->symb u) (prot/->symb v)))
           (and (every? (comp some? prot/->number) [u v])
                (== (prot/to-base-number u) (prot/to-base-number v))))))

(defn- compare-units [x y]
  (if (same-measure? x y)
    (compare (- (prot/to-base-number x) (prot/to-base-number y)) 0)
    (throw (ex-info "Cannot compare units of different measure." {:x x :y y}))))

;; Basic Unit

(deftype Unit [measure symb scale-of-base trans-of-base number]
  Object
  (toString [this] (str number))
  (hashCode [this] (hash-unit this))
  (equals [this other] (equal-units? this other))

  Comparable
  (compareTo [this other] (compare-units this other))

  prot/IUnit
  (->measure [_] measure)
  (->number [_] number)
  (->symb [_] symb)
  (to-base-number [_] (scale (- number trans-of-base) scale-of-base))
  (from-base-number [this n] (prot/with-num this (+ (scale n (/ 1 scale-of-base)) trans-of-base)))
  (with-num [_ n] (new Unit measure symb scale-of-base trans-of-base n)))


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

(defn numerators [units] (filter (comp pos? second) units))
(defn denominators [units] (filter (comp neg? second) units))

(defn- derived-symbol [units]
  (let [numerators (numerators units)
        denominators (denominators units)]
    (if (empty? denominators)
      (d-symbol-part numerators)
      (->> (map d-symbol-part [numerators denominators])
           (string/join ":")))))

(defn- repeated-numer-denom [units]
  [(mapcat (fn [[k v]] (repeat v k)) (numerators units))
   (mapcat (fn [[k v]] (repeat (- v) k)) (denominators units))])

(deftype Derived [measure units symb number]
  Object
  (toString [this] (str number))
  (hashCode [this] (hash-unit this))
  (equals [this other] (equal-units? this other))

  Comparable
  (compareTo [this other] (compare-units this other))

  prot/IUnit
  (->measure [_] measure)
  (->number [_] number)
  (->symb [_] (if symb symb (derived-symbol units)))
  (to-base-number [_] (let [[numerators denominators] (repeated-numer-denom units)]
                        (as-> number $
                              (reduce (fn [n u] (prot/to-base-number (prot/with-num u n))) $ numerators)
                              (reduce (fn [n u] (prot/->number (prot/from-base-number u n))) $ denominators))))
  (from-base-number [this n] (let [[numerators denominators] (repeated-numer-denom units)]
                               (as-> n $
                                     (reduce (fn [n u] (prot/->number (prot/from-base-number u n))) $ numerators)
                                     (reduce (fn [n u] (prot/to-base-number (prot/with-num u n))) $ denominators)
                                     (prot/with-num this $))))
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
  (let [derived (derive-units x y op)]
    (cond
      (empty? derived) (op (prot/->number x) (prot/->number y))

      (@registry derived)
      (prot/from-base-number (@registry derived) (op (prot/to-base-number x) (prot/to-base-number y)))

      :else (throw (ex-info (str "No derived unit is registered for " (derived-symbol derived))
                            derived)))))

(defn boxed-arithmetic [x y op]
  (cond
    (and (number? x) (number? y))
    (op x y)

    (and (unit? x) (number? y))
    (prot/with-num x (op (prot/->number x) y))

    (and (unit? y) (number? x))
    (prot/with-num y (op (prot/->number y) x))

    (= op *)
    (attempt-derivation x y op)

    (= op /)
    (if (same-measure? x y)
      (attempt-derivation x (convert y x) op)
      (attempt-derivation x y op))

    (or (= op +) (= op -))
    (if (same-measure? x y)
      (prot/from-base-number x (op (prot/to-base-number x) (prot/to-base-number y)))
      (throw (ex-info (str "Cannot add/subtract " x " and " y) {:from x :to y})))

    :else (throw (ex-info "Unsupported operation." {:op op :x x :y y}))))

(defn ensure-basic [units]
  (let [units (update-keys units #(if (fn? %) (%) %))]
    (->> units
         (reduce (fn [acc [k v]]
                   (merge-with + acc
                               (if (instance? Derived k)
                                 (ensure-basic (update-vals (.units k) #(* % v)))
                                 {k v})))
                 {}))))

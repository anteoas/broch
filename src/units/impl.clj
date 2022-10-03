(ns units.impl
  (:require [clojure.string :as string]))

(defprotocol IUnit
  (->measure [this])
  (->symbol [this])
  (->number [this])
  (->units [this])
  (to-base-number [this])
  (from-base-number [this n])
  (with-num [this n]))

(defn unit? [x] (satisfies? IUnit x))
(defn same-measure? [x y] (and (unit? x) (unit? y) (= (->measure x) (->measure y))))
(defn same-unit? [x y] (and (same-measure? x y) (= (->symbol x) (->symbol y))))
(defn- convert [a b] (from-base-number b (to-base-number a)))

(defn- hash-unit [u] (hash {:measure (->measure u) :symb (->symbol u) :number (to-base-number u)}))

(defn- equal-units? [u v]
  (and (same-measure? u v)
       (or (and (->number u) (->number v) (== (to-base-number u) (to-base-number v)))
           (and (nil? (->number u)) (nil? (->number v)) (same-unit? u v)))))

(defn- compare-units [x y]
  (if (same-measure? x y)
    (compare (to-base-number x) (to-base-number y))
    (throw (ex-info "Cannot compare units of different measure." {:x x :y y}))))

;; Basic Unit

(deftype Unit [measure symb scale-of-base trans-of-base number]
  Object
  (toString [_] (str number))
  (hashCode [this] (hash-unit this))
  (equals [this other] (equal-units? this other))

  Comparable
  (compareTo [this other] (compare-units this other))

  IUnit
  (->measure [_] measure)
  (->number [_] number)
  (->symbol [_] symb)
  (->units [this] {(with-num this nil) 1})
  (to-base-number [_] (when number (* (+ number trans-of-base) scale-of-base)))
  (from-base-number [this n] (with-num this (when n (- (/ n scale-of-base) trans-of-base))))
  (with-num [_ n] (new Unit measure symb scale-of-base trans-of-base n)))


;; Derived Unit

(defn- unit-symbol [unit ^Integer exponent]
  (let [symb (name (->symbol unit))
        exp (Math/abs exponent)]
    (if (= 1 exp)
      symb
      (str symb "^" exp))))

(defn- d-symbol-part [unit-seq]
  (->> (sort-by (comp ->symbol first) unit-seq)
       (map (partial apply unit-symbol))
       (string/join)))

(defn numerators [units] (filter (comp pos? second) units))
(defn denominators [units] (filter (comp neg? second) units))

(defn- derived-symbol [units]
  (let [numerators (numerators units)
        denominators (denominators units)]
    (if (empty? denominators)
      (d-symbol-part numerators)
      (->> (map d-symbol-part [numerators denominators])
           (string/join "/")))))

(defn- repeated-numer-denom [units]
  [(mapcat (fn [[k v]] (repeat v k)) (numerators units))
   (mapcat (fn [[k v]] (repeat (- v) k)) (denominators units))])

(deftype Derived [measure units symb number]
  Object
  (toString [_] (str number))
  (hashCode [this] (hash-unit this))
  (equals [this other] (equal-units? this other))

  Comparable
  (compareTo [this other] (compare-units this other))

  IUnit
  (->measure [_] measure)
  (->number [_] number)
  (->symbol [_] (or symb (derived-symbol units)))
  (->units [_] units)
  (to-base-number [_] (let [[numerators denominators] (repeated-numer-denom units)]
                        (as-> number $
                              (reduce (fn [n u] (to-base-number (with-num u n))) $ numerators)
                              (reduce (fn [n u] (->number (from-base-number u n))) $ denominators))))
  (from-base-number [this n] (let [[numerators denominators] (repeated-numer-denom units)]
                               (as-> n $
                                     (reduce (fn [n u] (->number (from-base-number u n))) $ numerators)
                                     (reduce (fn [n u] (to-base-number (with-num u n))) $ denominators)
                                     (with-num this $))))
  (with-num [_ n] (new Derived measure units symb n)))


;; Registry

(def symbol-reg (atom {}))
(def unit-reg (atom {}))

(defn register-unit! [unit]
  (swap! unit-reg assoc (->units unit) unit)
  (if (@symbol-reg (->symbol unit))
    (binding [*out* *err*]
      (println "WARN: a unit with symbol" (->symbol unit) "already exists!"))
    (swap! symbol-reg assoc (->symbol unit) unit)))


;; Operations

(defn- derive-units [x y op]
  (let [units-x (->units x)
        units-y (cond-> (->units y)
                        (= / op) (update-vals -))]
    (->> (merge-with + units-x units-y)
         (filter (comp not zero? second))
         (into {}))))

(defn attempt-derivation [x y op]
  (let [derived-units (derive-units x y op)]
    (cond
      (empty? derived-units) (op (->number x) (->number y))

      (@unit-reg derived-units)
      (from-base-number (@unit-reg derived-units) (op (to-base-number x) (to-base-number y)))

      :else (throw (ex-info (str "No derived unit is registered for " (derived-symbol derived-units))
                            derived-units)))))

(defn boxed-arithmetic [x y op]
  (cond
    (and (number? x) (number? y))
    (op x y)

    (and (unit? x) (number? y))
    (with-num x (op (->number x) y))

    (and (number? x) (unit? y))
    (with-num y (op (->number y) x))

    (or (= op *) (= op /))
    (if (and (same-measure? x y) (not (same-unit? x y)))
      (attempt-derivation x (convert y x) op)
      (attempt-derivation x y op))

    (or (= op +) (= op -))
    (if (same-measure? x y)
      (from-base-number x (op (to-base-number x) (to-base-number y)))
      (throw (ex-info (str "Cannot add/subtract " (->measure x) " and " (->measure y)) {:from x :to y})))

    :else (throw (ex-info "Unsupported operation." {:op op :x x :y y}))))


;; Construction

(defn new-unit [new-u x]
  (cond
    (nil? x)
    new-u

    (number? x)
    (with-num new-u x)

    (string? x)
    (let [n (read-string x)]
      (if (number? n)
        (with-num new-u n)
        (throw (ex-info (str "Can't make units with other things than numbers: " x) {:x x}))))

    (unit? x)
    (if (same-measure? x new-u)
      (convert x new-u)
      (throw (ex-info (str "Cannot convert a " (->measure x) " into " (->measure new-u))
                      {:from x :to new-u})))

    :else (throw (ex-info "Unhandled case." {:unit new-u :x x}))))

(defn ensure-basic [units]
  (let [units (update-keys units #(if (fn? %) (%) %))]
    (->> units
         (reduce (fn [acc [k v]]
                   (merge-with + acc
                               (if (instance? Derived k)
                                 (ensure-basic (update-vals (->units k) #(* % v)))
                                 {k v})))
                 {}))))

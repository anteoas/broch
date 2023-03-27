(ns broch.impl
  (:refer-clojure :exclude [symbol])
  (:require [broch.protocols :refer [IQuantity composition measure number symbol]]))

(defn quantity? [x] (satisfies? IQuantity x))
(defn- same-measure? [x y] (and (quantity? x) (quantity? y) (= (measure x) (measure y))))
(defn- same-unit? [x y] (and (same-measure? x y) (= (symbol x) (symbol y))))

(declare ->base)
(deftype Quantity [-measure -symbol -composition -number]
  Object
  (toString [_] (str -number " " -symbol))
  (hashCode [_] (hash {:measure -measure :symbol -symbol :number -number}))
  (equals [this other]
    (and (same-measure? this other)
         (or (and -number (number other) (== (->base this) (->base other)))
             (and (same-unit? this other) (nil? -number) (nil? (number other))))))

  Comparable
  (compareTo [this other]
    (if (same-measure? this other)
      (compare (->base this) (->base other))
      (throw (ex-info (str "Cannot compare units of different measure. " -measure " and " (measure other) ".")
                      {:x this :y other}))))

  IQuantity
  (measure [_] -measure)
  (number [_] -number)
  (symbol [_] -symbol)
  (composition [this]
    (if (empty? (dissoc -composition :broch/scaled))
      (assoc -composition this 1)
      -composition)))

(defn- quantity* [unit n] (->Quantity (measure unit) (symbol unit) (composition unit) n))
(defn boxed [f q] (quantity* q (f (number q))))
(defn- simple? [q] (empty? (dissoc (composition q) :broch/scaled (quantity* q nil))))

(defn- pow [n x] (reduce * (repeat x n)))

(defn- scale-of-base [q]
  (let [{:broch/keys [scaled] :as composition} (composition q)
        scaled (or (rationalize scaled) 1)]
    (if (simple? q)
      scaled
      (reduce (fn [x [k v]]
                (if (pos? v)
                  (* x (pow (scale-of-base k) v))
                  (/ x (pow (scale-of-base k) (- v)))))
              scaled
              (dissoc composition :broch/scaled)))))

(defn- downcast
  "Downcast if possible without losing precision."
  [n]
  (cond-> n
    (and (ratio? n) (= n (rationalize (unchecked-double n)))) (double)
    (== n (unchecked-long n)) (long)))

(defn- safe-scale [n m] (when (and n m) (* n m)))
(defn- ->base [q] (safe-scale (rationalize (number q)) (scale-of-base q)))
(defn- <-base [q n] (quantity* q (safe-scale n (/ 1 (scale-of-base q)))))

(defn- convert [a b]
  (let [converted (<-base b (->base a))]
    (cond
      (ratio? (number a)) converted
      (float? (number a)) (quantity* converted (double (number converted)))
      :else (quantity* converted (downcast (number converted))))))

(defn- converting-op [unit a b op]
  (let [converted (<-base unit (op (->base a) (->base b)))]
    (cond
      (ratio? (number a)) converted
      (float? (number a)) (quantity* converted (double (number converted)))
      :else (quantity* converted (downcast (number converted))))))

(defn quantity
  [unit x]
  (cond
    (nil? x)
    unit

    (number? x)
    (quantity* unit x)

    (string? x)
    (let [n (read-string x)]
      (if (number? n)
        (quantity* unit n)
        (throw (ex-info (str "Must be a number: " x) {:number x}))))

    (quantity? x)
    (if (same-measure? x unit)
      (convert x unit)
      (throw (ex-info (str "Cannot convert " (measure x) " into " (measure unit))
                      {:from x :to unit})))

    :else (throw (ex-info "Unhandled case." {:unit unit :x x}))))

(defonce symbol-registry (atom {}))
(defonce composition-registry (atom {}))

(def ^:dynamic *warn-on-symbol-collision* true)

(defn- warn-on-collision! [unit]
  (when (@symbol-registry (symbol unit))
    (binding [*out* *err*]
      (println "WARN: a unit with symbol" (symbol unit) "already exists! Overriding..."))))

(defn- measure-composition [unit-composition]
  (->> (dissoc unit-composition :broch/scaled)
       (map (fn [[k v]] {(measure k) v}))
       (apply merge-with +)))

(defn register-unit! [unit]
  (when *warn-on-symbol-collision* (warn-on-collision! unit))
  (swap! composition-registry (fn [reg]
                                (merge {(composition unit) unit}
                                       {(measure-composition (composition unit)) unit}
                                       reg
                                       (when (and (simple? unit) (= 1 (:broch/scaled (composition unit))))
                                         ; override "default" unit for measure
                                         {(measure-composition (composition unit)) unit}))))
  (swap! symbol-registry assoc (symbol unit) unit))


(defn- ensure-basic [comp]
  (->> (update-keys comp #(if (fn? %) (%) %))
       (reduce (fn [acc [k v]]
                 (if (= :broch/scaled k)
                   (merge-with * acc {k v})
                   (merge-with + acc
                               (if (simple? k)
                                 {k v}
                                 (ensure-basic (update-vals (composition k) #(* % v)))))))
               {})))

(defn unit [measure symbol composition]
  (->Quantity measure symbol (ensure-basic composition) nil))

(defonce symbol-registry (atom {}))
(defonce composition-registry (atom {}))

(def ^:dynamic *warn-on-symbol-collision* true)

(defn- warn-on-collision! [unit]
  (when (@symbol-registry (symbol unit))
    (binding [*out* *err*]
      (println "WARN: a unit with symbol" (symbol unit) "already exists! Overriding..."))))

(defn- measure-composition [unit-composition]
  (->> (dissoc unit-composition :broch/scaled)
       (map (fn [[k v]] {(measure k) v}))
       (apply merge-with +)))

(defn register-unit! [unit]
  (when *warn-on-symbol-collision* (warn-on-collision! unit))
  (swap! composition-registry (fn [reg]
                                (merge {(composition unit) unit}
                                       {(measure-composition (composition unit)) unit}
                                       reg)))
  (swap! symbol-registry assoc (symbol unit) unit))

(defn- derive-comp [x y op]
  (->> (reduce (fn [acc [k v]]
                 (cond
                   (and (= / op) (= :broch/scaled k)) (merge-with * acc {k (/ 1 v)})
                   (= :broch/scaled k) (merge-with * acc {k v})
                   (= / op) (merge-with + acc {k (- v)})
                   :else (merge-with + acc {k v})))
               (composition x) (composition y))
       (filter (comp not zero? second))
       (into {})))

(defn- attempt-derivation [x y op]
  (let [derived-comp (derive-comp x y op)]
    (cond
      (empty? derived-comp) (op (number x) (number y))

      (@composition-registry derived-comp)
      (converting-op (@composition-registry derived-comp) x y op)

      (@composition-registry (measure-composition derived-comp))
      (converting-op (@composition-registry (measure-composition derived-comp)) x y op)

      :else (throw (ex-info (str "No unit is registered for " derived-comp) derived-comp)))))

(defn boxed-arithmetic [x y op]
  (cond
    (and (number? x) (number? y))
    (op x y)

    (and (quantity? x) (number? y))
    (quantity x (op (number x) y))

    (and (number? x) (quantity? y))
    (quantity y (op x (number y)))

    (or (= op *) (= op /))
    (if (and (same-measure? x y) (not (same-unit? x y)))
      (attempt-derivation x (convert y x) op)
      (attempt-derivation x y op))

    (or (= op +) (= op -) (= op min) (= op max))
    (if (same-measure? x y)
      (<-base x (op (->base x) (->base y)))
      (throw (ex-info (str "Cannot add/subtract " (measure x) " and " (measure y)) {:from x :to y})))

    :else (throw (ex-info "Unsupported operation." {:op op :x x :y y}))))

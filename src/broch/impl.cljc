(ns broch.impl
  (:refer-clojure :exclude [* add - / symbol read-string rationalize])
  (:require [broch.protocols :refer [IQuantity composition measure number symbol]]
            [broch.numbers :refer [add sub mul div neg]]
            #?@(:cljs [[cljs.reader]
                       [cljs.compiler]]))
  #?(:clj (:import (clojure.lang IMeta IObj Obj)
                   (java.io Writer))))

(defn quantity? [x] (satisfies? IQuantity x))
(defn same-measure? [x y] (and (quantity? x) (quantity? y) (= (measure x) (measure y))))
(defn compatible? [x y] (or (same-measure? x y) (= (dissoc (composition x)) (composition y))))
(defn same-unit? [x y] (and (same-measure? x y) (= (symbol x) (symbol y))))

(declare ->base)
(defn- compare-quantities [x y]
  (if (same-measure? x y)
    (compare (->base x) (->base y))
    (throw (ex-info (str "Cannot compare units of different measure. " (measure x) " and " (measure y) ".")
                    {:x x :y y}))))
(defn- quantities-equal? [x y]
  (and (same-measure? x y)
       (or (and (number x) (number y) (let [bx (->base x) by (->base y)]
                                        (or (= bx by) (== bx by))))
           (and (same-unit? x y) (nil? (number x)) (nil? (number y))))))
(deftype Quantity [-measure -symbol -composition -number]
  #?@(:clj
      [Object
       (toString [_] (str -number " " -symbol))
       (hashCode [_] (hash {:measure -measure :symbol -symbol :number -number}))
       (equals [this other] (quantities-equal? this other))
       Comparable
       (compareTo [this other] (compare-quantities this other))]
      :cljs
      [Object
       (toString [_] (str -number " " -symbol))
       (equiv [this other] (-equiv this other))
       IEquiv
       (-equiv [this other] (quantities-equal? this other))
       IHash
       (-hash [this] (hash {:measure -measure :symbol -symbol :number -number}))
       IComparable
       (-compare [this other] (compare-quantities this other))])

  IQuantity
  (measure [_] -measure)
  (number [_] -number)
  (symbol [_] -symbol)
  (composition [_]
    (if (empty? (dissoc -composition :broch/scaled))
      (assoc -composition -measure 1)
      -composition)))

(defn- quantity* [unit n] (->Quantity (measure unit) (symbol unit) (composition unit) n))
(defn boxed [f q] (quantity* q (f (number q))))
(defn simple? [q] (= 1 (get (dissoc (composition q) :broch/scaled) (measure q))))

(defn- safe-scale [n m] (when (and n m) (mul n m)))
(defn- scale-of-base [q] (or (:broch/scaled (composition q)) 1))
(defn- ->base [q] (safe-scale (number q) (scale-of-base q)))
(defn- <-base [q n] (quantity* q (safe-scale n (div 1 (scale-of-base q)))))

(defn- convert [a b]
  (let [converted (<-base b (->base a))]
    (cond
      #?(:clj (ratio? (number a)) :cljs nil) converted
      (nil? (number a)) converted
      (float? (number a)) (quantity* converted (double (number converted)))
      :else (quantity* converted (number converted)))))

(defn- converting-op [unit a b op]
  (let [converted (<-base unit (op (->base a) (->base b)))]
    (cond
      #?(:clj (ratio? (number a)) :cljs nil) converted
      (float? (number a)) (quantity* converted (double (number converted)))
      :else (quantity* converted (number converted)))))

(def read-string #?(:clj clojure.core/read-string :cljs cljs.reader/read-string))

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
    (if (compatible? x unit)
      (convert x unit)
      (throw (ex-info (str "Cannot convert " (measure x) " into " (measure unit))
                      {:from x :to unit})))

    :else (throw (ex-info (str "Unhandled case: " x " " (type x)) {:unit unit :x x}))))

(defn- pow [n x]
  (if (neg? x)
    (div 1 (reduce mul (repeat (abs x) n)))
    (reduce mul (repeat x n))))

(defn- ensure-basic [unit-comp]
  (->> (update-keys unit-comp #(if (fn? %) (%) %))
       (map (fn [[k v]]
              (cond
                (= :broch/scaled k) {k v}
                (simple? k) {(measure k) v :broch/scaled (pow (scale-of-base k) v)}
                :else (into {} (map (fn [[i j]]
                                      {i (if (= :broch/scaled i)
                                           (pow j v)
                                           (mul j v))})
                                    (composition k))))))
       (reduce (fn [acc m]
                 (reduce (fn [acc [k v]]
                           (if (= :broch/scaled k)
                             (merge-with mul acc {k v})
                             (merge-with add acc {k v})))
                         acc m))
               {})
       (remove (fn [[_ v]] (= 0 v)))
       (into {})))

(defn unit [measure symbol composition]
  (->Quantity measure symbol (ensure-basic composition) nil))

(defonce symbol-registry (atom {}))
(defonce composition-registry (atom {}))

(def ^:dynamic *warn-on-symbol-collision* true)

(defn- warn-on-collision! [unit]
  (when (@symbol-registry (symbol unit))
    (let [error-str (str "WARN: a unit with symbol " (symbol unit) " already exists! Overriding...")]
      #?(:clj  (binding [*out* *err*] (println error-str))
         :cljs (js/console.error error-str)))))

(defn register-unit! [unit]
  (when *warn-on-symbol-collision* (warn-on-collision! unit))
  (swap! composition-registry (fn [reg] (cond-> reg
                                          (not (contains? reg (composition unit)))
                                          (assoc (composition unit) unit))))
  (swap! symbol-registry assoc (symbol unit) unit))

(defn- derive-comp [x y op]
  (->> (reduce (fn [acc [k v]]
                 (cond
                   (and (= div op) (= :broch/scaled k)) (merge-with mul acc {k (div 1 v)})
                   (= :broch/scaled k) (merge-with mul acc {k v})
                   (= div op) (merge-with add acc {k (neg v)})
                   :else (merge-with add acc {k v})))
               (composition x) (composition y))
       (filter (comp not zero? second))
       (into {})))

(defn- attempt-derivation [x y op]
  (let [derived-comp          (derive-comp x y op)
        unscaled-derived-comp (assoc derived-comp :broch/scaled 1)]
    (cond
      (or (empty? derived-comp) (empty? (dissoc derived-comp :broch/scaled))) (op (number x) (number y))

      (@composition-registry derived-comp)
      (converting-op (@composition-registry derived-comp) x y op)

      (@composition-registry unscaled-derived-comp)
      (converting-op (@composition-registry unscaled-derived-comp) x y op)

      :else (throw (ex-info (str "No unit is registered for " derived-comp) derived-comp)))))

(defn- unitless-quantity [number]
  (reify IQuantity
    (number [_] number)
    (symbol [_] nil)
    (measure [_] nil)
    (composition [_] {:broch/scaled 1})))

(defn boxed-arithmetic [x y op]
  (assert (or (quantity? x) (number? x)) (or (quantity? y) (number? y)))
  (cond
    (or (= op mul) (= op div))
    (let [x (cond-> x (number? x) (unitless-quantity))
          y (cond-> y (number? y) (unitless-quantity))]
      (if (and (compatible? x y) (not (same-unit? x y)))
        (attempt-derivation x (convert y x) op)
        (attempt-derivation x y op)))

    (or (= op add) (= op sub) (= op min) (= op max))
    (cond
      (and (number? x) (number? y)) (op x y)
      (and (quantity? x) (number? y)) (quantity x (op (number x) y))
      (and (number? x) (quantity? y)) (quantity y (op x (number y)))
      (compatible? x y) (converting-op x x y op)
      :else (throw (ex-info (str "Cannot add/subtract/compare " (measure x) " and " (measure y)) {:from x :to y})))

    :else (throw (ex-info "Unsupported operation." {:op op :x x :y y}))))


;; Data literal

(defn to-edn [q] [(number q) (symbol q)])
(defn from-edn [[n s]]
  (if (@symbol-registry s)
    (quantity (@symbol-registry s) n)
    (throw (ex-info (str "Symbol \"" s "\" not registered!") {:number n :symbol s :registry @symbol-registry}))))
(defn print-quantity [q] (str "#broch/quantity" (to-edn q)))

(def tags {'broch/quantity from-edn})
#?(:cljs (cljs.reader/register-tag-parser! 'broch/quantity #(from-edn %)))
(defn- ratio-emit [r]
  #?(:clj (if (ratio? r)
            (broch.numbers/->JSRatio (long (numerator r)) (long (denominator r)))
            r)))
(defmethod cljs.compiler/emit-constant* Quantity [q]
   (cljs.compiler/emits "new broch.impl.Quantity(")
   (cljs.compiler/emits-keyword (measure q))
   (cljs.compiler/emits ",")
   (cljs.compiler/emits (str \" (symbol q) \"))
   (cljs.compiler/emits ",")
   (cljs.compiler/emit-constant (update-vals (composition q) ratio-emit))
   (cljs.compiler/emits ",")
   (cljs.compiler/emit-constant (ratio-emit (number q)))
   (cljs.compiler/emits ")"))


#?(:clj
   (do
     (extend-protocol clojure.core.protocols/Datafiable
       Quantity
       (datafy [q] (to-edn q)))
     (defmethod print-method Quantity [q ^Writer w] (.write w ^String (print-quantity q)))
     (defmethod print-dup Quantity [q ^Writer w] (.write w ^String (print-quantity q))))
   :cljs
   (extend-protocol IPrintWithWriter
     Quantity
     (-pr-writer [q writer _] (-write writer (print-quantity q)))))

(ns broch.core
  (:refer-clojure :exclude [* + - / < <= > >= max min num symbol])
  (:require [broch.data :as data]
            [broch.impl :as impl]
            [broch.protocols :as p]))

;;
;; Operations on quantities
;; most of these also work on numbers directly
;;

(defn quantity?
  "Is this a quantity?"
  [u] (impl/quantity? u))

(defn measure
  "What this quantity is a measure of."
  [u] (when-not (number? u) (p/measure u)))

(defn symbol
  "The unit symbol for this quantity."
  [u] (when-not (number? u) (p/symbol u)))

(defn num
  "Get the number from a quantity. Pass through if already a number."
  [u] (if (number? u) u (p/number u)))

(defn with-num
  "Make copy of a quantity with a different number."
  [unit n] (impl/quantity unit n))

(defn boxed
  "Transform the quantity's number by any fn (i.e. fmap on the quantity-functor).
  Also works for numbers."
  [f x]
  (if (number? x)
    (f x)
    (impl/boxed f x)))

(defn box
  "Like boxed but partial."
  [f] (fn [x] (boxed f x)))

(defn +
  ([x] x)
  ([x y] (impl/boxed-arithmetic x y clojure.core/+))
  ([x y & more] (reduce + (+ x y) more)))

(defn -
  ([x] (boxed clojure.core/- x))
  ([x y] (impl/boxed-arithmetic x y clojure.core/-))
  ([x y & more] (reduce - (- x y) more)))

(defn *
  ([x] x)
  ([x y] (impl/boxed-arithmetic x y clojure.core/*))
  ([x y & more] (reduce * (* x y) more)))

(defn /
  ([x] x)
  ([x y] (impl/boxed-arithmetic x y clojure.core//))
  ([x y & more] (reduce / (/ x y) more)))

(defn min
  ([x] x)
  ([x y] (impl/boxed-arithmetic x y clojure.core/min))
  ([x y & more] (reduce min (min x y) more)))

(defn max
  ([x] x)
  ([x y] (impl/boxed-arithmetic x y clojure.core/max))
  ([x y & more] (reduce max (max x y) more)))

(defn <
  ([_] true)
  ([x y] (neg? (compare x y)))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defn >
  ([_] true)
  ([x y] (pos? (compare x y)))
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defn <=
  ([_] true)
  ([x y] (or (< x y) (= x y)))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn >=
  ([_] true)
  ([x y] (or (> x y) (= x y)))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

;; Serializing
(defn from-edn [x] (data/from-edn x))
(defn to-edn [u] (data/to-edn u))

;;
;; Creating units
;;

(defn new-unit
  "Register a new type of unit with the given measure, symbol, composition and/or scaling.
  Returns a fn (fn [number]) that creates a quantity of this unit."
  ([measure symb scale-or-comp]
   {:pre  [(keyword? measure) (string? symb) (or (number? scale-or-comp) (map? scale-or-comp))]
    :post [(fn? %)]}
   (let [composition (if (number? scale-or-comp) {:broch/scaled scale-or-comp} scale-or-comp)
         unit        (impl/unit measure symb composition)]
     (impl/register-unit! unit)
     (fn
       ([] unit)
       ([x] (impl/quantity unit x)))))
  ([measure symb scaling composition]
   (new-unit measure symb (assoc composition :broch/scaled scaling))))

(defmacro defunit
  ([unit-fn-name measure symb scale-or-comp]
   `(def ~unit-fn-name (new-unit ~measure ~symb ~scale-or-comp)))
  ([unit-fn-name measure symb scaling composition]
   `(def ~unit-fn-name (new-unit ~measure ~symb ~scaling ~composition))))

;;
;; Unit definitions
;;

;; Length
(defunit millimeters :length "mm" 1/1000)
(defunit centimeters :length "cm" 1/100)
(defunit decimeters :length "dm" 1/10)
(defunit meters :length "m" 1)
(defunit kilometers :length "km" 1000)

;; Stupid Length
(defunit yards :length "yd" 0.9144)
(defunit miles :length "mi" 1760 {yards 1})
(defunit feet :length "ft" 1/3 {yards 1})
(defunit inches :length "in" 1/12 {feet 1})
(defunit nautical-miles :length "NM" 1852)

;; Time
(defunit seconds :time "s" 1)
(defunit minutes :time "min" 60)
(defunit hours :time "h" 3600)

;; Mass
(defunit grams :mass "g" 1/1000)
(defunit kilograms :mass "kg" 1)
(defunit tonnes :mass "t" 1000)

(defunit kilograms-per-second :mass-rate "kg/s" {kilograms 1 seconds -1})
(defunit kilograms-per-hour :mass-rate "kg/h" {kilograms 1 hours -1})
(defunit tonnes-per-hour :mass-rate "t/h" {tonnes 1 hours -1})

;; Other SI
(defunit kelvin :thermodynamic-temperature "K" 1)
(defunit amperes :electric-current "A" 1)
(defunit moles :amount-of-substance "mol" 1)
(defunit candelas :luminous-intensity "cd" 1)

;; Area
(defunit square-millimeters :area "mm²" {millimeters 2})
(defunit square-centimeters :area "cm²" {centimeters 2})
(defunit square-decimeters :area "dm²" {decimeters 2})
(defunit square-meters :area "m²" {meters 2})
(defunit square-kilometers :area "km²" {kilometers 2})
(defunit square-miles :area "mi²" {miles 2})
(defunit square-yards :area "yd²" {yards 2})
(defunit square-feet :area "ft²" {feet 2})
(defunit square-inches :area "in²" {inches 2})
(defunit acres :area "ac" 4840 {yards 2})

;; Volume
(defunit cubic-millimeters :volume "cm³" {millimeters 3})
(defunit cubic-centimeters :volume "dm³" {centimeters 3})
(defunit cubic-meters :volume "m³" {meters 3})
(defunit cubic-kilometers :volume "km³" {kilometers 3})
(defunit cubic-miles :volume "mi³" {miles 3})
(defunit cubic-yards :volume "yd³" {yards 3})
(defunit cubic-feet :volume "ft³" {feet 3})
(defunit cubic-inches :volume "in³" {inches 3})

(defunit liters :volume "l" {decimeters 3})
(defunit deciliters :volume "dl" 1/10 {liters 1})
(defunit centiliters :volume "cl" 1/100 {liters 1})

(defunit liters-per-hour :volume-rate "l/h" {liters 1 hours -1})
(defunit liters-per-second :volume-rate "l/s" {liters 1 seconds -1})

;; Speed
(defunit kilometers-per-hour :speed "km/h" {kilometers 1 hours -1})
(defunit meters-per-second :speed "m/s" {meters 1 seconds -1})
(defunit miles-per-hour :speed "mi/h" {miles 1 hours -1})
(defunit knots :speed "kn" {nautical-miles 1 hours -1})

;; Acceleration
(defunit meters-per-second2 :acceleration "m/s²" {meters 1 seconds -2})

;; Force
(defunit newtons :force "N" {kilograms 1 meters 1 seconds -2})

;; Power & Energy
(defunit joules :energy "J" {newtons 1 meters 1})
(defunit watts :power "W" {joules 1 seconds -1})
(defunit kilowatts :power "kW" 1000 {watts 1})
(defunit watt-hours :energy "Wh" {watts 1 hours 1})
(defunit kilowatt-hours :energy "kWh" 1000 {watts 1 hours 1})


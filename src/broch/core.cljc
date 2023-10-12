(ns broch.core
  (:refer-clojure :exclude [* + - / < <= > >= max min num symbol])
  (:require [broch.impl :as impl]
            [broch.extensions]
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
(defn from-edn [x] (impl/from-edn x))
(defn to-edn [u] (impl/to-edn u))

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
  "Can be used to define units, but it's not compatible with cljs.
  Alternatively use the form this expands to:
  (def unit-fn-name (new-unit measure symbol scaling composition))"
  ([unit-fn-name measure symb scale-or-comp]
   `(def ~unit-fn-name (new-unit ~measure ~symb ~scale-or-comp)))
  ([unit-fn-name measure symb scaling composition]
   `(def ~unit-fn-name (new-unit ~measure ~symb ~scaling ~composition))))

(defmacro defunit-once
  "Can be used to define units, but it's not compatible with cljs.
  Alternatively use the form this expands to:
  (defonce unit-fn-name (new-unit measure symbol scaling composition))"
  ([unit-fn-name measure symb scale-or-comp]
   `(defonce ~unit-fn-name (new-unit ~measure ~symb ~scale-or-comp)))
  ([unit-fn-name measure symb scaling composition]
   `(defonce ~unit-fn-name (new-unit ~measure ~symb ~scaling ~composition))))

;;
;; Unit definitions
;;

;; Length
(def millimeters (new-unit :length "mm" (/ 1 1000)))
(def centimeters (new-unit :length "cm" (/ 1 100)))
(def decimeters (new-unit :length "dm" (/ 1 10)))
(def meters (new-unit :length "m" 1))
(def kilometers (new-unit :length "km" 1000))

(def yards (new-unit :length "yd" 0.9144))
(def miles (new-unit :length "mi" {:broch/scaled 1760 yards 1}))
(def feet (new-unit :length "ft" {:broch/scaled (/ 1 3) yards 1}))
(def inches (new-unit :length "in" {:broch/scaled (/ 1 12) feet 1}))
(def nautical-miles (new-unit :length "NM" 1852))

;; Time
(def nanoseconds (new-unit :time "ns" 1.0E-9))
(def microseconds (new-unit :time "μs" 1.0E-6))
(def milliseconds (new-unit :time "ms" 1.0E-3))
(def seconds (new-unit :time "s" 1))
(def minutes (new-unit :time "min" 60))
(def hours (new-unit :time "h" 3600))

;; Mass
(def picogram (new-unit :mass "pg" 1.0E-15))
(def nanogram (new-unit :mass "ng" 1.0E-12))
(def micrograms (new-unit :mass "µg" 1.0E-9))
(def milligrams (new-unit :mass "mg" 1.0E-6))
(def grams (new-unit :mass "g" 1.0E-3))
(def kilograms (new-unit :mass "kg" 1))
(def tonnes (new-unit :mass "t" 1.0E3))

(def pounds (new-unit :mass "lbs" 0.45359237))
(def ounce (new-unit :mass "oz" (/ 1 16) {pounds 1}))
(def long-tons (new-unit :mass "ST" 2240 {pounds 1}))
(def short-tons (new-unit :mass "LT" 2000 {pounds 1}))

;; Other SI
(def kelvin (new-unit :thermodynamic-temperature "K" 1))
(def amperes (new-unit :electric-current "A" 1))
(def moles (new-unit :amount-of-substance "mol" 1))
(def candelas (new-unit :luminous-intensity "cd" 1))

;; Mass rate
(def kilograms-per-second (new-unit :mass-rate "kg/s" {kilograms 1 seconds -1}))
(def kilograms-per-hour (new-unit :mass-rate "kg/h" {kilograms 1 hours -1}))
(def tonnes-per-hour (new-unit :mass-rate "t/h" {tonnes 1 hours -1}))

;; Area
(def square-millimeters (new-unit :area "mm²" {millimeters 2}))
(def square-centimeters (new-unit :area "cm²" {centimeters 2}))
(def square-decimeters (new-unit :area "dm²" {decimeters 2}))
(def square-meters (new-unit :area "m²" {meters 2}))
(def square-kilometers (new-unit :area "km²" {kilometers 2}))

(def square-miles (new-unit :area "mi²" {miles 2}))
(def square-yards (new-unit :area "yd²" {yards 2}))
(def square-feet (new-unit :area "ft²" {feet 2}))
(def square-inches (new-unit :area "in²" {inches 2}))
(def acres (new-unit :area "ac" 4840 {yards 2}))

;; Volume
(def cubic-millimeters (new-unit :volume "cm³" {millimeters 3}))
(def cubic-centimeters (new-unit :volume "dm³" {centimeters 3}))
(def cubic-meters (new-unit :volume "m³" {meters 3}))
(def cubic-kilometers (new-unit :volume "km³" {kilometers 3}))

(def cubic-miles (new-unit :volume "mi³" {miles 3}))
(def cubic-yards (new-unit :volume "yd³" {yards 3}))
(def cubic-feet (new-unit :volume "ft³" {feet 3}))
(def cubic-inches (new-unit :volume "in³" {inches 3}))

(def liters (new-unit :volume "l" {decimeters 3}))
(def deciliters (new-unit :volume "dl" (/ 1 10) {liters 1}))
(def centiliters (new-unit :volume "cl" (/ 1 100) {liters 1}))

;; Volume Rate
(def liters-per-hour (new-unit :volume-rate "l/h" {liters 1 hours -1}))
(def liters-per-second (new-unit :volume-rate "l/s" {liters 1 seconds -1}))

;; Surface Density
(def micrograms-per-square-millimeter (new-unit :surface-density "µg/mm²" {micrograms 1 square-millimeters -1}))
(def grams-per-square-centimeter (new-unit :surface-density "g/cm²" {grams 1 square-centimeters -1}))
(def kilograms-per-square-centimeter (new-unit :surface-density "kg/cm²" {kilograms 1 square-centimeters -1}))
(def kilograms-per-square-meter (new-unit :surface-density "kg/m²" {kilograms 1 square-meters -1}))

(def ounces-per-square-inch (new-unit :surface-density "oz/in²" {ounce 1 square-inches -1}))
(def pounds-per-square-inch (new-unit :surface-density "lbs/in²" {pounds 1 square-inches -1}))
(def pounds-per-square-foot (new-unit :surface-density "lbs/ft²" {pounds 1 square-feet -1}))

;; Density
(def grams-per-cubic-centimeter (new-unit :density "g/m³" {grams 1 cubic-centimeters -1}))
(def kilograms-per-cubic-meter (new-unit :density "kg/m³" {kilograms 1 cubic-meters -1}))
(def kilograms-per-liter (new-unit :density "kg/l" {kilograms 1 liters -1}))

(def pounds-per-cubic-foot (new-unit :density "lbs/ft³" {pounds 1 cubic-feet -1}))

;; Speed
(def kilometers-per-hour (new-unit :speed "km/h" {kilometers 1 hours -1}))
(def meters-per-second (new-unit :speed "m/s" {meters 1 seconds -1}))
(def miles-per-hour (new-unit :speed "mi/h" {miles 1 hours -1}))
(def knots (new-unit :speed "kn" {nautical-miles 1 hours -1}))

;; Acceleration
(def meters-per-second2 (new-unit :acceleration "m/s²" {meters 1 seconds -2}))

;; Force
(def newtons (new-unit :force "N" {kilograms 1 meters 1 seconds -2}))

;; Power & Energy
(def joules (new-unit :energy "J" {newtons 1 meters 1}))
(def watts (new-unit :power "W" {joules 1 seconds -1}))
(def kilowatts (new-unit :power "kW" 1000 {watts 1}))
(def watt-hours (new-unit :energy "Wh" {watts 1 hours 1}))
(def kilowatt-hours (new-unit :energy "kWh" 1000 {watts 1 hours 1}))


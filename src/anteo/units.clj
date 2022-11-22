(ns anteo.units
  (:refer-clojure :exclude [* + - / < <= > >= num symbol])
  (:require [anteo.units.impl :as impl :refer [->Unit ->Derived]]
            [anteo.units.data :as data]))

;; Operations on Units

(defn unit?
  "Is this a unit?"
  [u] (impl/unit? u))

(defn measure
  "What this unit is a measure of."
  [u] (impl/->measure u))

(defn symbol
  "The symbol for this unit."
  [u] (impl/->symbol u))

(defn num
  "Get just the number. Pass through if already a number."
  [x] (if (number? x) x (impl/->number x)))

(defn with-num
  "Make copy of unit with a different number."
  [unit n] (impl/with-num unit n))

(defn box
  "Call any fn on the number directly. Returns same unit."
  [f] #(with-num % (f (num %))))

(defn +
  ([x] x)
  ([x y] (impl/boxed-arithmetic x y clojure.core/+))
  ([x y & more] (reduce + (+ x y) more)))

(defn -
  ([x] ((box clojure.core/-) x))
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

;; Defining units

(defn new-unit
  "Register a new unit with given measure symbol and scaling.
  Returns a unit-fn (fn [number]) that creates a new unit from a number."
  ([measure symb] (new-unit measure symb nil))
  ([measure symb scale-of-base]
   {:pre  [(keyword? measure) (string? symb) (or (nil? scale-of-base) (number? scale-of-base))]
    :post [(fn? %)]}
   (let [unit (->Unit measure symb (or scale-of-base 1) nil)]
     (impl/register-unit! unit)
     (fn
       ([] unit)
       ([x] (impl/new-unit unit x))))))

(defn new-derived
  "Register a new derived unit with the given measure and unit composition as a map of unit-fns to exponents.
  Returns a unit-fn (fn [number]) that creates a new unit from a number."
  ([measure units symb] (new-derived measure units symb nil))
  ([measure units symb scale-of-base]
   {:pre  [(keyword? measure) (map? units) (string? symb)
           (or (nil? scale-of-base) (number? scale-of-base))]
    :post [(fn? %)]}
   (let [units (impl/ensure-basic units)
         derived (->Derived measure units symb scale-of-base nil)]
     (assert (and (every? impl/unit? (keys units))
                  (every? int? (vals units))))
     (impl/register-unit! derived)
     (fn
       ([] derived)
       ([x] (impl/new-unit derived x))))))


;; Length
(def millimeters (new-unit :length "mm" 1/1000))
(def centimeters (new-unit :length "cm" 1/100))
(def decimeters (new-unit :length "dm" 1/10))
(def meters (new-unit :length "m" 1))
(def kilometers (new-unit :length "km" 1000))
(def nautical-miles (new-unit :length "NM" 1852))

;; Stupid Length
(def miles (new-unit :length "mi" 1609.344))
(def yards (new-unit :length "yd" 0.9144))
(def feet (new-unit :length "ft" 0.3048))
(def inches (new-unit :length "in" 0.0254))

;; Time
(def seconds (new-unit :time "s" 1))
(def minutes (new-unit :time "min" 60))
(def hours (new-unit :time "h" 3600))

;; Mass
(def grams (new-unit :mass "g" 1/1000))
(def kilograms (new-unit :mass "kg" 1))
(def tonnes (new-unit :mass "t" 1000))

(def kilograms-per-second (new-derived :mass-rate {kilograms 1 seconds -1} "kg/s"))
(def kilograms-per-hour (new-derived :mass-rate {kilograms 1 hours -1} "kg/h"))
(def tonnes-per-hour (new-derived :mass-rate {tonnes 1 hours -1} "t/h"))

;; Other SI
(def kelvin (new-unit :thermodynamic-temperature "K" 1))
(def amperes (new-unit :electric-current "A" 1))
(def moles (new-unit :amount-of-substance "mol" 1))
(def candelas (new-unit :luminous-intensity "cd" 1))

;; Area
(def squared-millimeters (new-derived :area {millimeters 2} "mm²"))
(def squared-centimeters (new-derived :area {centimeters 2} "cm²"))
(def squared-decimeters (new-derived :area {decimeters 2} "dm²"))
(def squared-meters (new-derived :area {meters 2} "m²"))
(def squared-kilometers (new-derived :area {kilometers 2} "km²"))
(def squared-miles (new-derived :area {miles 2} "mi²"))
(def squared-yards (new-derived :area {yards 2} "yd²"))
(def squared-feet (new-derived :area {feet 2} "ft²"))
(def squared-inches (new-derived :area {inches 2} "in²"))

;; Volume
(def cubed-millimeters (new-derived :volume {millimeters 3} "cm³"))
(def cubed-centimeters (new-derived :volume {centimeters 3} "dm³"))
(def cubed-meters (new-derived :volume {meters 3} "m³"))
(def cubed-kilometers (new-derived :volume {kilometers 3} "km³"))
(def cubed-miles (new-derived :volume {miles 3} "mi³"))
(def cubed-yards (new-derived :volume {yards 3} "yd³"))
(def cubed-feet (new-derived :volume {feet 3} "ft³"))
(def cubed-inches (new-derived :volume {inches 3} "in³"))

(def liters (new-derived :volume {decimeters 3} "l"))
(def deciliters (new-derived :volume {liters 1} "dl" 1/10))
(def centiliters (new-derived :volume {liters 1} "cl" 1/100))

(def liters-per-hour (new-derived :volume-rate {liters 1 hours -1} "l/h"))
(def liters-per-second (new-derived :volume-rate {liters 1 seconds -1} "l/s"))

;; Speed
(def kilometers-per-hour (new-derived :speed {kilometers 1 hours -1} "km/h"))
(def meters-per-second (new-derived :speed {meters 1 seconds -1} "m/s"))
(def miles-per-hour (new-derived :speed {miles 1 hours -1} "mi/h"))
(def knots (new-derived :speed {nautical-miles 1 hours -1} "kn"))

;; Acceleration
(def meters-per-second2 (new-derived :acceleration {meters 1 seconds -2} "m/s²"))

;; Force
(def newtons (new-derived :force {kilograms 1 meters 1 seconds -2} "N"))

;; Power & Energy
(def joules (new-derived :energy {newtons 1 meters 1} "J"))
(def watts (new-derived :power {joules 1 seconds -1} "W"))
(def kilowatts (new-derived :power {watts 1} "kW" 1000))
(def watt-hours (new-derived :energy {watts 1 hours 1} "Wh"))
(def kilowatt-hours (new-derived :energy {watts 1 hours 1} "kWh" 1000))


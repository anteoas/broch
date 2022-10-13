(ns anteo.units
  (:refer-clojure :exclude [* + - / < <= > >= num])
  (:require [anteo.units.impl :as impl :refer [->Unit ->Derived]]
            [anteo.units.data-literals]))

;; Operations on Units

(defn unit?
  "Is this a unit?"
  [u] (impl/unit? u))

(defn measure
  "What this unit is a measure of."
  [u] (impl/->measure u))

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

;; Defining units

(defn unit
  ([measure symb scale-of-base]
   (unit measure symb scale-of-base 0))
  ([measure symb scale-of-base trans-of-base]
   {:pre  [(keyword? measure) (string? symb) (number? scale-of-base) (number? trans-of-base)]
    :post [(fn? %)]}
   (let [unit (->Unit measure symb scale-of-base trans-of-base nil)]
     (impl/register-unit! unit)
     (fn
       ([] unit)
       ([x] (impl/new-unit unit x))))))

(defn derived
  ([measure units] (derived measure units nil))
  ([measure units symb]
   {:pre  [(keyword? measure) (map? units) (or (nil? symb) (string? symb))]
    :post [(fn? %)]}
   (let [units (impl/ensure-basic units)
         derived (->Derived measure units symb nil)]
     (assert (and (every? impl/unit? (keys units))
                  (every? int? (vals units))))
     (impl/register-unit! derived)
     (fn
       ([] derived)
       ([x] (impl/new-unit derived x))))))


;; Units
(def centi (unit :amount "c" 1/100))
(def deci (unit :amount "d" 1/10))
(def kilo (unit :amount "k" 1000))

;; Length
(def millimeters (unit :length "mm" 1/1000))
(def centimeters (unit :length "cm" 1/100))
(def decimeters (unit :length "dm" 1/10))
(def meters (unit :length "m" 1))
(def kilometers (unit :length "km" 1000))
(def nautical-miles (unit :length "NM" 1852))

;; Stupid Length
(def miles (unit :length "mi" 1609.344))
(def yards (unit :length "yd" 0.9144))
(def feet (unit :length "ft" 0.3048))
(def inches (unit :length "in" 0.0254))

;; Time
(def seconds (unit :time "s" 1))
(def minutes (unit :time "min" 60))
(def hours (unit :time "h" 3600))

;; Mass
(def grams (unit :mass "g" 1/1000))
(def kilograms (unit :mass "kg" 1))
(def tonnes (unit :mass "t" 1000))

(def kilograms-per-second (derived :mass-rate {kilograms 1 seconds -1}))
(def kilograms-per-hour (derived :mass-rate {kilograms 1 hours -1}))
(def tonnes-per-hour (derived :mass-rate {tonnes 1 hours -1}))

;; Temperature
(def kelvin (unit :thermodynamic-temperature "K" 1))
(def celsius (unit :thermodynamic-temperature "C" 1 273.15))
(def fahrenheit (unit :thermodynamic-temperature "F" 5/9 459.67))


;; Other SI
(def amperes (unit :electric-current "A" 1))
(def moles (unit :amount-of-substance "mol" 1))
(def candelas (unit :luminous-intensity "cd" 1))

;; Area
(def squared-millimeters (derived :area {millimeters 2}))
(def squared-centimeters (derived :area {centimeters 2}))
(def squared-decimeters (derived :area {decimeters 2}))
(def squared-meters (derived :area {meters 2}))
(def squared-kilometers (derived :area {kilometers 2}))
(def squared-miles (derived :area {miles 2}))
(def squared-yards (derived :area {yards 2}))
(def squared-feet (derived :area {feet 2}))
(def squared-inches (derived :area {inches 2}))

;; Volume
(def cubed-millimeters (derived :volume {millimeters 3}))
(def cubed-centimeters (derived :volume {centimeters 3}))
(def cubed-meters (derived :volume {meters 3}))
(def cubed-kilometers (derived :volume {kilometers 3}))
(def cubed-miles (derived :volume {miles 3}))
(def cubed-yards (derived :volume {yards 3}))
(def cubed-feet (derived :volume {feet 3}))
(def cubed-inches (derived :volume {inches 3}))

(def liters (derived :volume {decimeters 3} "l"))
(def deciliters (derived :volume {deci 1 liters 1} "dl"))
(def centiliters (derived :volume {centi 1 liters 1} "cl"))

(def liters-per-hour (derived :volume-rate {liters 1 hours -1}))
(def liters-per-second (derived :volume-rate {liters 1 seconds -1}))

;; Speed
(def kilometers-per-hour (derived :speed {kilometers 1 hours -1}))
(def meters-per-second (derived :speed {meters 1 seconds -1}))
(def miles-per-hour (derived :speed {miles 1 hours -1}))
(def knots (derived :speed {nautical-miles 1 hours -1} "kn"))

;; Acceleration
(def meters-per-second2 (derived :acceleration {meters 1 seconds -2}))

;; Force
(def newtons (derived :force {kilograms 1 meters 1 seconds -2} "N"))

;; Power & Energy
(def joules (derived :energy {newtons 1 meters 1} "J"))
(def watts (derived :power {joules 1 seconds -1} "W"))
(def kilowatts (derived :power {kilo 1 watts 1} "kW"))
(def watt-hours (derived :energy {watts 1 hours 1} "Wh"))
(def kilowatt-hours (derived :energy {kilowatts 1 hours 1} "kWh"))


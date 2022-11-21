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
  [measure symb scale-of-base]
  {:pre  [(keyword? measure) (string? symb) (number? scale-of-base)]
   :post [(fn? %)]}
  (let [unit (->Unit measure symb scale-of-base nil)]
    (impl/register-unit! unit)
    (fn
      ([] unit)
      ([x] (impl/new-unit unit x)))))

(defn new-derived
  "Register a new derived unit with the given measure and unit composition as a map of unit-fns to exponents.
  Returns a unit-fn (fn [number]) that creates a new unit from a number."
  ([measure units] (new-derived measure units nil))
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
(def centi (new-unit :amount "c" 1/100))
(def deci (new-unit :amount "d" 1/10))
(def kilo (new-unit :amount "k" 1000))

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

(def kilograms-per-second (new-derived :mass-rate {kilograms 1 seconds -1}))
(def kilograms-per-hour (new-derived :mass-rate {kilograms 1 hours -1}))
(def tonnes-per-hour (new-derived :mass-rate {tonnes 1 hours -1}))

;; Other SI
(def kelvin (new-unit :thermodynamic-temperature "K" 1))
(def amperes (new-unit :electric-current "A" 1))
(def moles (new-unit :amount-of-substance "mol" 1))
(def candelas (new-unit :luminous-intensity "cd" 1))

;; Area
(def squared-millimeters (new-derived :area {millimeters 2}))
(def squared-centimeters (new-derived :area {centimeters 2}))
(def squared-decimeters (new-derived :area {decimeters 2}))
(def squared-meters (new-derived :area {meters 2}))
(def squared-kilometers (new-derived :area {kilometers 2}))
(def squared-miles (new-derived :area {miles 2}))
(def squared-yards (new-derived :area {yards 2}))
(def squared-feet (new-derived :area {feet 2}))
(def squared-inches (new-derived :area {inches 2}))

;; Volume
(def cubed-millimeters (new-derived :volume {millimeters 3}))
(def cubed-centimeters (new-derived :volume {centimeters 3}))
(def cubed-meters (new-derived :volume {meters 3}))
(def cubed-kilometers (new-derived :volume {kilometers 3}))
(def cubed-miles (new-derived :volume {miles 3}))
(def cubed-yards (new-derived :volume {yards 3}))
(def cubed-feet (new-derived :volume {feet 3}))
(def cubed-inches (new-derived :volume {inches 3}))

(def liters (new-derived :volume {decimeters 3} "l"))
(def deciliters (new-derived :volume {deci 1 liters 1} "dl"))
(def centiliters (new-derived :volume {centi 1 liters 1} "cl"))

(def liters-per-hour (new-derived :volume-rate {liters 1 hours -1}))
(def liters-per-second (new-derived :volume-rate {liters 1 seconds -1}))

;; Speed
(def kilometers-per-hour (new-derived :speed {kilometers 1 hours -1}))
(def meters-per-second (new-derived :speed {meters 1 seconds -1}))
(def miles-per-hour (new-derived :speed {miles 1 hours -1}))
(def knots (new-derived :speed {nautical-miles 1 hours -1} "kn"))

;; Acceleration
(def meters-per-second2 (new-derived :acceleration {meters 1 seconds -2}))

;; Force
(def newtons (new-derived :force {kilograms 1 meters 1 seconds -2} "N"))

;; Power & Energy
(def joules (new-derived :energy {newtons 1 meters 1} "J"))
(def watts (new-derived :power {joules 1 seconds -1} "W"))
(def kilowatts (new-derived :power {kilo 1 watts 1} "kW"))
(def watt-hours (new-derived :energy {watts 1 hours 1} "Wh"))
(def kilowatt-hours (new-derived :energy {kilowatts 1 hours 1} "kWh"))


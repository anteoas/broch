(ns anteo.broch
  (:refer-clojure :exclude [* + - / < <= > >= num symbol min max])
  (:require [anteo.broch.impl :as impl :refer [->Unit ->Derived]]
            [anteo.broch.protocols :as p]
            [anteo.broch.data :as data]))

;;
;; Operations on Units
;; most of these also work on numbers directly
;;

(defn unit?
  "Is this a unit?"
  [u] (impl/unit? u))

(defn measure
  "What this unit is a measure of."
  [u] (when-not (number? u) (p/->measure u)))

(defn symbol
  "The symbol for this unit."
  [u] (when-not (number? u) (p/->symbol u)))

(defn num
  "Get just the number. Pass through if already a number."
  [x] (if (number? x) x (p/->number x)))

(defn with-num
  "Make copy of unit with a different number."
  [unit n] (p/with-num unit n))

(defn box
  "Transform the units number by any fn (i.e. fmap on the unit-functor).
  Also works for numbers."
  [f] (fn [x]
        (if (number? x)
          (f x)
          (with-num x (f (num x))))))

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
  "Register a new type of unit with the given measure, symbol and scaling.
  Returns a fn (fn [number]) that creates a new unit of this type."
  [measure symb scale-of-base]
  {:pre  [(keyword? measure) (string? symb) (number? scale-of-base)]
   :post [(fn? %)]}
  (let [unit (->Unit measure symb (rationalize scale-of-base) nil)]
    (impl/register-unit! unit)
    (fn
      ([] unit)
      ([x] (impl/new-unit unit x)))))

(defn new-derived
  "Register a new type of derived unit with the given measure, symbol, unit composition, and optional scaling.
  Returns a fn (fn [number]) that creates a new unit of this type."
  [measure units symb scale-of-base]
  {:pre  [(keyword? measure) (map? units) (string? symb) (or (nil? scale-of-base) (number? scale-of-base))]
   :post [(fn? %)]}
  (let [units   (impl/ensure-basic (cond-> units
                                     scale-of-base (assoc :scaled (rationalize scale-of-base))))
        derived (->Derived measure units symb nil)]
    (impl/register-unit! derived)
    (fn
      ([] derived)
      ([x] (impl/new-unit derived x)))))

(defmacro defunit [unit-fn-name measure symb scale-of-base]
  `(def ~unit-fn-name (new-unit ~measure ~symb ~scale-of-base)))

(defmacro defderived
  ([unit-fn-name measure units symb]
   `(def ~unit-fn-name (new-derived ~measure ~units ~symb nil)))
  ([unit-fn-name measure units symb scale-of-base]
   `(def ~unit-fn-name (new-derived ~measure ~units ~symb ~scale-of-base))))

;;
;; Unit definitions
;;

;; Length
(defunit millimeters :length "mm" 1/1000)
(defunit centimeters :length "cm" 1/100)
(defunit decimeters :length "dm" 1/10)
(defunit meters :length "m" 1)
(defunit kilometers :length "km" 1000)
(defunit nautical-miles :length "NM" 1852)

;; Stupid Length
(defunit miles :length "mi" 1609.344)
(defunit yards :length "yd" 1250/1367)
(defunit feet :length "ft" 1250/4101)
(defunit inches :length "in" 100/3937)

;; Time
(defunit seconds :time "s" 1)
(defunit minutes :time "min" 60)
(defunit hours :time "h" 3600)

;; Mass
(defunit grams :mass "g" 1/1000)
(defunit kilograms :mass "kg" 1)
(defunit tonnes :mass "t" 1000)

(defderived kilograms-per-second :mass-rate {kilograms 1 seconds -1} "kg/s")
(defderived kilograms-per-hour :mass-rate {kilograms 1 hours -1} "kg/h")
(defderived tonnes-per-hour :mass-rate {tonnes 1 hours -1} "t/h")

;; Other SI
(defunit kelvin :thermodynamic-temperature "K" 1)
(defunit amperes :electric-current "A" 1)
(defunit moles :amount-of-substance "mol" 1)
(defunit candelas :luminous-intensity "cd" 1)

;; Area
(defderived squared-millimeters :area {millimeters 2} "mm²")
(defderived squared-centimeters :area {centimeters 2} "cm²")
(defderived squared-decimeters :area {decimeters 2} "dm²")
(defderived squared-meters :area {meters 2} "m²")
(defderived squared-kilometers :area {kilometers 2} "km²")
(defderived squared-miles :area {miles 2} "mi²")
(defderived squared-yards :area {yards 2} "yd²")
(defderived squared-feet :area {feet 2} "ft²")
(defderived squared-inches :area {inches 2} "in²")

;; Volume
(defderived cubed-millimeters :volume {millimeters 3} "cm³")
(defderived cubed-centimeters :volume {centimeters 3} "dm³")
(defderived cubed-meters :volume {meters 3} "m³")
(defderived cubed-kilometers :volume {kilometers 3} "km³")
(defderived cubed-miles :volume {miles 3} "mi³")
(defderived cubed-yards :volume {yards 3} "yd³")
(defderived cubed-feet :volume {feet 3} "ft³")
(defderived cubed-inches :volume {inches 3} "in³")

(defderived liters :volume {decimeters 3} "l")
(defderived deciliters :volume {liters 1} "dl" 1/10)
(defderived centiliters :volume {liters 1} "cl" 1/100)

(defderived liters-per-hour :volume-rate {liters 1 hours -1} "l/h")
(defderived liters-per-second :volume-rate {liters 1 seconds -1} "l/s")

;; Speed
(defderived kilometers-per-hour :speed {kilometers 1 hours -1} "km/h")
(defderived meters-per-second :speed {meters 1 seconds -1} "m/s")
(defderived miles-per-hour :speed {miles 1 hours -1} "mi/h")
(defderived knots :speed {nautical-miles 1 hours -1} "kn")

;; Acceleration
(defderived meters-per-second2 :acceleration {meters 1 seconds -2} "m/s²")

;; Force
(defderived newtons :force {kilograms 1 meters 1 seconds -2} "N")

;; Power & Energy
(defderived joules :energy {newtons 1 meters 1} "J")
(defderived watts :power {joules 1 seconds -1} "W")
(defderived kilowatts :power {watts 1} "kW" 1000)
(defderived watt-hours :energy {watts 1 hours 1} "Wh")
(defderived kilowatt-hours :energy {watts 1 hours 1} "kWh" 1000)


(ns units.core
  (:refer-clojure :exclude [* + - / < <= > >=])
  (:require [units.impl :as impl :refer [->Unit ->Derived]]
            [units.protocols :as prot]
            [clojure.string :as string])
  (:import (units.impl Unit)))

;; Operations on Units

(defn box
  "Call any fn on the number directly. Returns same unit."
  [f] #(prot/with-num % (f (prot/->number %))))

(defn- boxed-arithmetic [x y op]
  (cond
    (and (number? x) (number? y))
    (op x y)

    (and (satisfies? prot/IUnit x) (number? y))
    ((box #(op % y)) x)

    (and (satisfies? prot/IUnit y) (number? x))
    ((box #(op % x)) y)

    (or (= op clojure.core/*) (= op clojure.core//))
    (impl/attempt-derivation x y op)

    (or (= op clojure.core/+) (= op clojure.core/-))
    (if (impl/same-measure? x y)
      (impl/from-base-number x (op (impl/to-base-number x) (impl/to-base-number y)))
      (throw (ex-info (str "Cannot add/subtract " x " and " y) {:from x :to y})))

    :else (throw (ex-info "Unsupported operation." {:op op :x x :y y}))))

(defn +
  ([x] x)
  ([x y] (boxed-arithmetic x y clojure.core/+))
  ([x y & more] (reduce + (+ x y) more)))

(defn -
  ([x] ((box clojure.core/-) x))
  ([x y] (boxed-arithmetic x y clojure.core/-))
  ([x y & more] (reduce - (- x y) more)))

(defn *
  ([x] x)
  ([x y] (boxed-arithmetic x y clojure.core/*))
  ([x y & more] (reduce * (* x y) more)))

(defn /
  ([x] x)
  ([x y] (boxed-arithmetic x y clojure.core//))
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

(defn- new-unit [new-u x]
  (cond
    (nil? x)
    new-u

    (number? x)
    (prot/with-num new-u x)

    (string? x)
    (prot/with-num new-u (impl/parse-number x))

    (and (satisfies? prot/IUnit x) (impl/same-measure? x new-u))
    (impl/convert x new-u)

    (and (satisfies? prot/IUnit x) (not (impl/same-measure? x new-u)))
    (throw (ex-info (str "Cannot convert a " x " into " new-u) {:fomr x :to new-u}))

    :else (throw (ex-info "Unhandled case." {:unit new-u :x x}))))

(defn ->unit [measure symb scale-of-base]
  (let [unit (->Unit measure symb scale-of-base nil)]
    (impl/register-unit! unit)
    (fn
      ([] unit)
      ([x] (new-unit unit x)))))

(defn ->derived
  ([measure units] (->derived measure units nil))
  ([measure units symb]
   (let [units (update-keys units #(if (fn? %) (%) %))
         derived (->Derived measure units symb nil)]
     (assert (and (every? #(instance? Unit %) (keys units))
                  (every? int? (vals units))))
     (impl/register-derived! derived)
     (fn
       ([] derived)
       ([x] (new-unit derived x))))))


;; Units

;; Length
(def millimeters (->unit :length :mm 1/1000))
(def centimeters (->unit :length :cm 1/100))
(def meters (->unit :length :m 1))
(def kilometers (->unit :length :km 1000))
(def nautical-miles (->unit :length :NM 1852))

;; Stupid Length
(def miles (->unit :length :mi 1609.344))
(def yards (->unit :length :yd 0.9144))
(def feet (->unit :length :ft 0.3048))
(def inches (->unit :length :in 0.0254))

;; Time
(def seconds (->unit :time :s 1))
(def minutes (->unit :time :min 60))
(def hours (->unit :time :h 3600))

;; Mass
(def grams (->unit :mass :g 1/1000))
(def kilograms (->unit :mass :kg 1))
(def tonnes (->unit :mass :t 1000))

;; Other SI
(def amperes (->unit :electric-current :A 1))
(def kelvins (->unit :thermodynamic-temperature :K 1))      ;; TODO allow translations as well
(def mols (->unit :amount-of-substance :mol 1))
(def candelas (->unit :luminous-intensity :cd 1))

;; Area
(def squared-millimeters (->derived :area {millimeters 2}))
(def squared-centimeters (->derived :area {centimeters 2}))
(def squared-meters (->derived :area {meters 2}))
(def squared-kilometers (->derived :area {kilometers 2}))
(def squared-miles (->derived :area {miles 2}))
(def squared-yards (->derived :area {yards 2}))
(def squared-feet (->derived :area {feet 2}))
(def squared-inches (->derived :area {inches 2}))

;; Speed
(def kilometers-per-hour (->derived :speed {kilometers 1 hours -1}))
(def meters-per-second (->derived :speed {meters 1 seconds -1}))
(def miles-per-hour (->derived :speed {miles 1 hours -1}))

;; Acceleration
(def meters-per-second2 (->derived :acceleration {meters 1 seconds -2}))

;; Force
(def newtons (->derived :force {meters 1 seconds -2} :N))


(comment
  ; Generate data-readers map

  (def unit-syms [#'millimeters #'centimeters #'meters #'kilometers #'nautical-miles #'miles #'yards #'feet #'inches
                  #'seconds #'minutes #'hours #'grams #'kilograms #'tonnes #'amperes #'kelvins #'mols #'candelas
                  #'kilometers #'meters #'miles #'meters #'newtons #'kilometers-per-hour #'meters-per-second
                  #'miles-per-hour #'meters-per-second2 #'squared-millimeters #'squared-centimeters #'squared-meters
                  #'squared-kilometers #'squared-miles #'squared-yards #'squared-feet #'squared-inches])

  (defn data-readers [unit-syms]
    (letfn [(unit-sym->symbol [u]
              (->> (str (u))
                   (drop-last 2)
                   (drop 1)
                   (string/join)
                   (symbol)))]
      (reduce (fn [acc u]
                (assoc acc (unit-sym->symbol u) (symbol u)))
              {} unit-syms)))

  (data-readers unit-syms)
  )

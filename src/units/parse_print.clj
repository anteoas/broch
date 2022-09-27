(ns units.parse-print
  (:require [units.protocols :refer [->symb ->measure]]
            [clojure.string :as string])
  (:import (java.io Writer)
           (units.impl Unit Derived)
           (clojure.lang Ratio)))

(defn- print-unit [u] (str "#" (name (->measure u)) "/" (name (->symb u)) " \"" u "\""))
(defmethod print-method Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-dup Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-method Derived [d ^Writer w] (.write w ^String (print-unit d)))
(defmethod print-dup Derived [d ^Writer w] (.write w ^String (print-unit d)))

(defn- parse-ratio [^String s]
  (let [svec (string/split s #"/")]
    (when (and (= 2 (count svec)) (every? parse-long svec))
      (let [[numerator denominator] svec]
        (new Ratio (biginteger numerator) (biginteger denominator))))))

(defn parse-number [^String s]
  (or (parse-long s) (parse-ratio s) (parse-double s) (biginteger s)))

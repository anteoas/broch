(ns units.data-literals
  (:require [units.protocols :as prot]
            [units.impl :as impl])
  (:import (units.impl Unit Derived)
           (java.io Writer)))

(defn to-edn [u] [(prot/->number u) (prot/->symb u)])
(defn print-unit [u] (str "#unit/u" (to-edn u)))

(defmethod print-method Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-dup Unit [u ^Writer w] (.write w ^String (print-unit u)))

(defmethod print-method Derived [d ^Writer w] (.write w ^String (print-unit d)))
(defmethod print-dup Derived [d ^Writer w] (.write w ^String (print-unit d)))

(defn from-edn [[n s]] (prot/with-num (@impl/symbol-reg s) n))
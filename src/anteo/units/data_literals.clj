(ns anteo.units.data-literals
  (:require [anteo.units.impl :as impl])
  (:import (anteo.units.impl Unit Derived)
           (java.io Writer)))

(defn to-edn [u] [(impl/->number u) (impl/->symbol u)])
(defn print-unit [u] (str "#unit/u" (to-edn u)))

(defmethod print-method Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-dup Unit [u ^Writer w] (.write w ^String (print-unit u)))

(defmethod print-method Derived [d ^Writer w] (.write w ^String (print-unit d)))
(defmethod print-dup Derived [d ^Writer w] (.write w ^String (print-unit d)))

(defn from-edn [[n s]]
  (if (@impl/symbol-reg s)
    (impl/with-num (@impl/symbol-reg s) n)
    (throw (ex-info (str "Symbol " s " not registered!") {:number n :symbol s :registry @impl/symbol-reg}))))
(ns anteo.units.data
  (:require [anteo.units.impl :as impl]
            [clojure.core.protocols :as core-protocols])
  (:import (anteo.units.impl Derived Unit)
           (java.io Writer)))


(defn from-edn [[n s]]
  (if (@impl/symbol-reg s)
    (impl/with-num (@impl/symbol-reg s) n)
    (throw (ex-info (str "Symbol " s " not registered!") {:number n :symbol s :registry @impl/symbol-reg}))))
(defn to-edn [u] [(impl/->number u) (impl/->symbol u)])
(defn- print-unit [u] (str "#unit/u" (to-edn u)))
(defmethod print-method Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-dup Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-method Derived [d ^Writer w] (.write w ^String (print-unit d)))
(defmethod print-dup Derived [d ^Writer w] (.write w ^String (print-unit d)))

(extend-protocol core-protocols/Datafiable
  Unit
  (datafy [u] (to-edn u))

  Derived
  (datafy [u] (to-edn u)))

(ns broch.data
  (:require [broch.impl :as impl]
            [broch.protocols :as p]
            [clojure.core.protocols :as core-protocols])
  (:import (broch.impl Quantity)
           (java.io Writer)))

(defn from-edn [[n s]]
  (if (@impl/symbol-registry s)
    (impl/quantity (@impl/symbol-registry s) n)
    (throw (ex-info (str "Symbol \"" s "\" not registered!") {:number n :symbol s :registry @impl/symbol-registry}))))
(defn to-edn [u] [(p/number u) (p/symbol u)])
(defn- print-unit [u] (str "#broch/quantity" (to-edn u)))
(defmethod print-method Quantity [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-dup Quantity [u ^Writer w] (.write w ^String (print-unit u)))

(extend-protocol core-protocols/Datafiable
  Quantity
  (datafy [u] (to-edn u)))

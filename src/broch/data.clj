(ns broch.data
  (:require [broch.impl :as impl]
            [broch.protocols :as p]
            [clojure.core.protocols :as core-protocols])
  (:import (broch.impl Derived Unit)
           (clojure.lang BigInt)
           (java.io Writer)))

(defn- downcast
  "Downcast BigInt to long or ratio to double if possible without losing precision."
  [n]
  (cond
    (or (instance? BigInt n) (instance? BigInteger n)) (try (long n) (catch Exception _ n))
    (and (ratio? n) (= n (rationalize (double n)))) (double n)
    :else n))

(defn from-edn [[n s]]
  (if (@impl/symbol-reg s)
    (p/with-num (@impl/symbol-reg s) n)
    (throw (ex-info (str "Symbol " s " not registered!") {:number n :symbol s :registry @impl/symbol-reg}))))
(defn to-edn [u] [(downcast (p/->number u)) (p/->symbol u)])
(defn- print-unit [u] (str "#broch/unit" (to-edn u)))
(defmethod print-method Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-dup Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-method Derived [d ^Writer w] (.write w ^String (print-unit d)))
(defmethod print-dup Derived [d ^Writer w] (.write w ^String (print-unit d)))

(extend-protocol core-protocols/Datafiable
  Unit
  (datafy [u] (to-edn u))

  Derived
  (datafy [u] (to-edn u)))

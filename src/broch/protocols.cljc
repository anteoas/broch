(ns broch.protocols
  (:refer-clojure :exclude [symbol]))

(defprotocol IQuantity
  (measure [this])
  (symbol [this])
  (number [this])
  (composition [this]))

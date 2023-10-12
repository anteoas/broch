(ns broch.extensions
  (:require [broch.protocols :refer [IQuantity]])
  #?(:clj (:import (java.time Duration))))

#?(:clj
   (extend-type Duration
     IQuantity
     (measure [_] :time)
     (symbol [_] "ns")
     (number [d] (.toNanos d))
     (composition [_] {:broch/scaled 1.0E-9, :time 1})))
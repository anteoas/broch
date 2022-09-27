(ns units.protocols)

(defprotocol IUnit
  (->measure [this])
  (->symb [this])
  (->number [this])
  (->scale-of-base [this])
  (with-num [this n]))
(ns units.protocols)

(defprotocol IUnit
  (->measure [this])
  (->symb [this])
  (->scale-of-base [this])
  (->number [this])
  (with-num [this n]))

(ns units.protocols)

(defprotocol IUnit
  (->measure [this])
  (->symb [this])
  (->number [this])
  (to-base-number [this])
  (from-base-number [this n])
  (with-num [this n]))
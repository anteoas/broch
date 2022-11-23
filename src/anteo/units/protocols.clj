(ns anteo.units.protocols)

(defprotocol IUnit
  (->measure [this])
  (->symbol [this])
  (->number [this])
  (->units [this])
  (to-base-number [this])
  (from-base-number [this n])
  (with-num [this n]))

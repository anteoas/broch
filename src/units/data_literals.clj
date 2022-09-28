(ns units.data-literals
  (:require [units.protocols :as prot]
            [units.impl :as impl]
            [clojure.string :as string]
            [clojure.pprint :as pp])
  (:import (units.impl Unit Derived)
           (java.io Writer)))

(defmethod print-method Unit [u ^Writer w] (.write w ^String (impl/print-unit u)))
(defmethod print-dup Unit [u ^Writer w] (.write w ^String (impl/print-unit u)))

(defmethod print-method Derived [d ^Writer w] (.write w ^String (impl/print-unit d)))
(defmethod print-dup Derived [d ^Writer w] (.write w ^String (impl/print-unit d)))

(comment
  ; Generate data-readers map
  (let [unit-syms (->> (ns-interns 'units.core)
                       (filter (fn [[k v]]
                                 (try (satisfies? prot/IUnit (v))
                                      (catch Exception _ false))))
                       (map second))
        unit-sym->symbol (fn [u]
                           (->> (str (u))
                                (drop-last 2)
                                (drop 1)
                                (string/join)
                                (symbol)))
        dr-map (reduce (fn [acc u]
                         (assoc acc (unit-sym->symbol u) (symbol u)))
                       (sorted-map) unit-syms)
        file "src/data_readers.clj"
        current  (read-string (slurp file))]
    (->> (merge current dr-map)
         (into (sorted-map))
         (pp/pprint)
         (with-out-str)
         (spit file)))
  )
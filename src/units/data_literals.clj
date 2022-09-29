(ns units.data-literals
  (:require [units.protocols :as prot]
            [clojure.string :as string]
            [clojure.pprint :as pp])
  (:import (units.impl Unit Derived)
           (java.io Writer)))

(defn print-unit [u]
  (str "#" (name (prot/->measure u)) "/" (name (prot/->symb u)) "\"" (prot/->number u) "\""))

(defmethod print-method Unit [u ^Writer w] (.write w ^String (print-unit u)))
(defmethod print-dup Unit [u ^Writer w] (.write w ^String (print-unit u)))

(defmethod print-method Derived [d ^Writer w] (.write w ^String (print-unit d)))
(defmethod print-dup Derived [d ^Writer w] (.write w ^String (print-unit d)))

(comment
  ; Generate data-readers map
  (let [unit-syms (->> (ns-interns 'units.core)
                       (filter (fn [[k v]]
                                 (try (satisfies? prot/IUnit (v))
                                      (catch Exception _ false))))
                       (map second))
        unit-sym->symbol (fn [u]
                           (->> (print-unit (u))
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
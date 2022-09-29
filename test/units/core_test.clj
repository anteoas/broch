(ns units.core-test
  (:require [clojure.test :refer [testing is]]
            [units.protocols :as prot]
            [units.core :as un]
            [units.impl :as impl]))

(defn fuzzy= [tolerance x y]
  (let [diff (Math/abs ^long (- x y))]
    (< diff tolerance)))

(def unit-fns (->> (ns-interns 'units.core)
                   (filter (fn [[_ v]]
                             (try (satisfies? prot/IUnit (v))
                                  (catch Exception _ false))))
                   (map second)))

(testing "units of same measure are invertible"
  (doseq [u unit-fns]
    (let [units-of-same-measure (filter #(impl/same-measure? (u) (%)) unit-fns)]
      (doseq [v units-of-same-measure]
        (is (fuzzy= 0.000001 (prot/->number (u 123)) (prot/->number (u (v (u 123))))))))))

(testing "readers"
  (doseq [u unit-fns]
    (let [unit (u 123)]
      (is (= unit (read-string (pr-str unit)))))))

(testing "comparison"
  (is (= #length/m"1000" #length/km"1"))
  (is (un/< #length/ft"1" #length/m"1"))
  (is (un/> #time/h"1" #time/min"1")))

(testing "simple arithmetic"

  (let [m (un/meters 134)
        f (un/feet 52)]
    (is (= #length/m"149.8496" (un/+ m f)))
    (is (= #length/m"118.1504" (un/- m f)))
    (is (= #length/m"402" (un/* m 3)))
    (is (= #length/m"134/3" (un// m 3)))))

(testing "complex arithmetic"
  (is (= #speed/m:s"3" (un// #length/m"9" #time/s"3")))
  (is (= #time/h"2" (un// #energy/Wh"4" #power/W"2")))
  (is (= #time/h"2" (un// #energy/kWh"4" (un/* #amount/k"1" #power/W"2"))))
  (is (= #energy/J"2" (un/* #force/N"1" #length/m"2"))))

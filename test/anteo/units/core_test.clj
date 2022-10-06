(ns anteo.units.core-test
  (:require [clojure.test :refer [testing is]]
            [anteo.units :as un]))

(defn fuzzy= [tolerance x y]
  (let [diff (Math/abs ^long (- x y))]
    (< diff tolerance)))

(def unit-fns (->> (ns-interns 'anteo.units)
                   (filter (fn [[_ v]]
                             (try (un/unit? (v))
                                  (catch Exception _ false))))
                   (map second)))

(testing "units of same measure are invertible"
  (doseq [u unit-fns]
    (let [units-of-same-measure (filter #(= (un/measure (u)) (un/measure (%))) unit-fns)]
      (doseq [v units-of-same-measure]
        (is (fuzzy= 0.000001 (un/num (u 123)) (un/num (u (v (u 123))))))))))

(testing "readers"
  (doseq [u unit-fns]
    (let [unit (u 123)]
      (is (= unit (read-string (pr-str unit)))))))

(testing "comparison"
  (is (= #unit/u[1000 "m"] #unit/u[1 "km"]))
  (is (un/< #unit/u[1 "ft"] #unit/u[1 "m"]))
  (is (un/> #unit/u[1 "h"] #unit/u[1 "min"])))

(testing "simple arithmetic"

  (let [m (un/meters 134)
        f (un/feet 52)]
    (is (= #unit/u[149.8496 "m"] (un/+ m f)))
    (is (= #unit/u[118.1504 "m"] (un/- m f)))
    (is (= #unit/u[402 "m"] (un/* m 3)))
    (is (= #unit/u[134/3 "m"] (un// m 3)))))

(testing "complex arithmetic"
  (is (= #unit/u[3 "m/s"] (un// #unit/u[9 "m"] #unit/u[3 "s"])))
  (is (= #unit/u[2 "h"] (un// #unit/u[4 "Wh"] #unit/u[2 "W"])))
  (is (= #unit/u[2 "h"] (un// #unit/u[4 "kWh"] (un/* #unit/u[1 "k"] #unit/u[2 "W"]))))
  (is (= #unit/u[2 "J"] (un/* #unit/u[1 "N"] #unit/u[2 "m"])))
  (is (= #unit/u[0 "m"] (un// 0 #unit/u[2 "m"]))))

(testing "temperature"
  (is (= (un/kelvin 273.15) (un/kelvin (un/celsius 0))))
  (is (= (un/* (un/kelvin 273.15) 2) (un/+ (un/celsius 0) (un/celsius 0)))))

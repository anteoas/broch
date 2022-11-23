(ns anteo.units-test
  (:require
   [anteo.units.impl :as impl]
   [clojure.test :refer [deftest testing is are]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [anteo.units :as un]))


(def unit-fns (->> (ns-interns 'anteo.units)
                   (filter (fn [[_ v]]
                             (try (un/unit? (v))
                                  (catch Exception _ false))))
                   (map second)))


(deftest readers
  (doseq [u unit-fns]
    (let [unit (u 123)]
      (is (= unit (read-string (pr-str unit)))))))

(deftest printing
  (are [x y] (= x y)
    "#unit/u[0.25 \"km\"]" (pr-str (un/kilometers 0.25))
    "#unit/u[0.25 \"km\"]" (pr-str (un/kilometers 1/4))
    "#unit/u[1/3 \"km\"]" (pr-str (un/kilometers 1/3))))

(deftest api
  (let [m (un/meters 123)]
    (is (un/unit? m))
    (are [x y] (= x y)
      :length (un/measure m)
      "m" (un/symbol m)
      123 (un/num m)
      1234 (un/num (un/with-num m 1234))
      #unit/u[124 "m"] ((un/box inc) m)
      (un/from-edn [123 "m"]) m
      [123 "m"] (un/to-edn m))))

(deftest invertible-conversion
  (doseq [u unit-fns]
    (let [units-of-same-measure (filter #(= (un/measure (u)) (un/measure (%))) unit-fns)]
      (doseq [v units-of-same-measure]
        (testing (str "Functions: \n" u "\n" v)
          (is (== (un/num (u 123)) (un/num (u (v (u 123)))))))))))

(deftest comparison
  (is (= #unit/u[1000 "m"] #unit/u[1 "km"]))
  (is (un/< #unit/u[1 "ft"] #unit/u[1 "m"]))
  (is (un/> #unit/u[1 "h"] #unit/u[1 "min"])))

(deftest arithmetic
  (let [m (un/meters 134)
        f (un/feet 52)]
    (are [x y] (= x y)
      #unit/u[614534/4101 "m"] (un/+ m f)
      #unit/u[484534/4101 "m"] (un/- m f)
      #unit/u[402 "m"] (un/* m 3)
      #unit/u[134/3 "m"] (un// m 3)
      #unit/u[3 "m/s"] (un// #unit/u[9 "m"] #unit/u[3 "s"])
      #unit/u[2 "h"] (un// #unit/u[4 "Wh"] #unit/u[2 "W"])
      #unit/u[2 "h"] (un// #unit/u[4 "kWh"] (un/* 1000 #unit/u[2 "W"]))
      #unit/u[2 "J"] (un/* #unit/u[1 "N"] #unit/u[2 "m"])
      #unit/u[0 "m"] (un// 0 #unit/u[2 "m"]))))

(defn- is-NaN?
  "Test if this number is nan"
  [x] (false? (= x x)))                                     ;; Nan is the only value for which equality is false

(defn- op-equal? [op n m]
  (let [a (un/meters n) b (un/meters m)
        x (op (impl/attempt-rationalize n) (impl/attempt-rationalize m))
        y (un/num ((symbol "un" (name op)) a b))]
    (or (= x y)
        (and (is-NaN? x) (is-NaN? y)))))

(defspec numeric-ops 100
  (prop/for-all [n gen/double m gen/double]
    (is (op-equal? '+ n m) (str n "+" m))
    (is (op-equal? '- n m) (str n "-" m))
    (is (op-equal? '* n m) (str n "*" m))
    (is (op-equal? '/ n m) (str n "/" m))
    (is (op-equal? '< n m) (str n "<" m))
    (is (op-equal? '> n m) (str n ">" m))
    (is (op-equal? '<= n m) (str n "<=" m))
    (is (op-equal? '>= n m) (str n ">=" m))))

(declare thrown?)
(deftest number-handling
  (is (not (un/unit? 123)))
  (is (nil? (un/measure 123)))
  (is (nil? (un/symbol 123)))
  (is (= 123 (un/num 123)))
  (is (thrown? IllegalArgumentException (un/with-num 1 123))))


(comment
  (clojure.test/run-tests))
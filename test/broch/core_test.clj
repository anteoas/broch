(ns broch.core-test
  (:require
   [broch.core :as b]
   [broch.impl :as impl]
   [clojure.test :refer [are deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))


(def unit-fns (->> (ns-interns 'broch.core)
                   (filter (fn [[_ v]]
                             (try (b/unit? (v))
                                  (catch Exception _ false))))
                   (map second)))


(deftest readers
  (doseq [u unit-fns]
    (let [unit (u 123)]
      (is (= unit (read-string (pr-str unit)))))))

(deftest printing
  (are [x y] (= x y)
    "#broch/unit[0.25 \"km\"]" (pr-str (b/kilometers 0.25))
    "#broch/unit[0.25 \"km\"]" (pr-str (b/kilometers 1/4))
    "#broch/unit[1/3 \"km\"]" (pr-str (b/kilometers 1/3))))

(deftest api
  (let [m (b/meters 123)]
    (is (b/unit? m))
    (are [x y] (= x y)
      :length (b/measure m)
      "m" (b/symbol m)
      123 (b/num m)
      1234 (b/num (b/with-num m 1234))
      #broch/unit[124 "m"] ((b/box inc) m)
      (b/from-edn [123 "m"]) m
      [123 "m"] (b/to-edn m))))

(deftest invertible-conversion
  (doseq [u unit-fns]
    (let [units-of-same-measure (filter #(= (b/measure (u)) (b/measure (%))) unit-fns)]
      (doseq [v units-of-same-measure]
        (testing (str "Functions: \n" u "\n" v)
          (is (== (b/num (u 123)) (b/num (u (v (u 123)))))))))))

(deftest comparison
  (is (= #broch/unit[1000 "m"] #broch/unit[1 "km"]))
  (is (b/< #broch/unit[1 "ft"] #broch/unit[1 "m"]))
  (is (b/> #broch/unit[1 "h"] #broch/unit[1 "min"])))

(deftest arithmetic
  (let [m (b/meters 134)
        f (b/feet 52)]
    (are [x y] (= x y)
      #broch/unit[614534/4101 "m"] (b/+ m f)
      #broch/unit[484534/4101 "m"] (b/- m f)
      #broch/unit[402 "m"] (b/* m 3)
      #broch/unit[134/3 "m"] (b// m 3)
      #broch/unit[3 "m/s"] (b// #broch/unit[9 "m"] #broch/unit[3 "s"])
      #broch/unit[2 "h"] (b// #broch/unit[4 "Wh"] #broch/unit[2 "W"])
      #broch/unit[2 "h"] (b// #broch/unit[4 "kWh"] (b/* 1000 #broch/unit[2 "W"]))
      #broch/unit[2 "J"] (b/* #broch/unit[1 "N"] #broch/unit[2 "m"])
      #broch/unit[0 "m"] (b// 0 #broch/unit[2 "m"]))))

(defn- is-NaN?
  "Test if this number is nan"
  [x] (false? (= x x)))                                     ;; Nan is the only value for which equality is false

(defn- op-equal? [op n m]
  (let [a (b/meters n) b (b/meters m)
        x (op (impl/attempt-rationalize n) (impl/attempt-rationalize m))
        y (b/num ((symbol "un" (name op)) a b))]
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
  (is (not (b/unit? 123)))
  (is (nil? (b/measure 123)))
  (is (nil? (b/symbol 123)))
  (is (= 123 (b/num 123)))
  (is (thrown? IllegalArgumentException (b/with-num 1 123))))


(deftest min-max
  (is (= (b/meters 10) (b/min (b/meters 10))))
  (is (= (b/meters 7) (b/min (b/meters 10) (b/meters 7))))
  (is (= (b/meters 7) (b/min (b/meters 10) 7)))

  (is (= (b/meters 10) (b/max (b/meters 10))))
  (is (= (b/meters 10) (b/max (b/meters 10) (b/meters 7))))
  (is (= (b/meters 10) (b/max (b/meters 10) 7)))

  (is (= (b/meters 10) (b/min (b/meters 10) (b/max (b/kilometers 7) (b/meters 8)))))
  (is (= (b/kilometers 7) (b/max (b/meters 10) (b/kilometers 7) 88)))
  )

;; TODO (/ #broch/unit[12 "J"] #broch/unit[1 "km"])

(comment
  (clojure.test/run-tests))

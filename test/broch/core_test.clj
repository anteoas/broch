(ns broch.core-test
  (:require
   [broch.core :as b]
   [clojure.test :refer [are deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))

(def unit-fns (->> (ns-interns 'broch.core)
                   (filter (fn [[_ v]]
                             (try (b/quantity? (v))
                                  (catch Exception _ false))))
                   (map second)))


(deftest readers
  (doseq [u unit-fns]
    (let [quantity (u 123)]
      (is (= quantity (read-string (pr-str quantity)))))))

(deftest printing
  (are [x y] (= x y)
    "#broch/quantity[0.25 \"km\"]" (pr-str (b/kilometers 0.25))
    "#broch/quantity[1/4 \"km\"]" (pr-str (b/kilometers 1/4))
    "#broch/quantity[1.852 \"km\"]" (pr-str (b/kilometers (b/nautical-miles 1)))))

(deftest api
  (testing "for units"
    (let [m (b/meters 123)]
      (is (b/quantity? m))
      (are [x y] (= x y)
        :length (b/measure m)
        "m" (b/symbol m)
        123 (b/num m)
        1234 (b/num (b/with-num m 1234))
        #broch/quantity[124 "m"] (b/boxed inc m)
        (b/from-edn [123 "m"]) m
        [123 "m"] (b/to-edn m))))
  (testing "for numbers"
    (let [n 123.456]
      (is (not (b/quantity? n)))
      (are [x y] (= x y)
        nil (b/measure n)
        nil (b/symbol n)
        n (b/num n)
        124.456 (b/boxed inc n)))))

(deftest invertible-conversion
  (doseq [u unit-fns]
    (let [units-of-same-measure (filter #(= (b/measure (u)) (b/measure (%))) unit-fns)]
      (doseq [v units-of-same-measure]
        (testing (str "Functions: \n" u "\n" v)
          (is (== (b/num (u 123)) (b/num (u (v (u 123)))))))))))

(deftest comparison
  (is (= #broch/quantity[1000 "m"] #broch/quantity[1 "km"]))
  (is (b/< #broch/quantity[1 "ft"] #broch/quantity[1 "m"]))
  (is (b/> #broch/quantity[1 "h"] #broch/quantity[1 "min"])))

(deftest arithmetic
  (let [m (b/meters 134)
        f (b/feet 52)]
    (are [x y] (= x y)
      #broch/quantity[614534/4101 "m"] (b/+ m f)
      #broch/quantity[484534/4101 "m"] (b/- m f)
      #broch/quantity[402 "m"] (b/* m 3)
      #broch/quantity[134/3 "m"] (b// m 3)
      #broch/quantity[3 "m/s"] (b// #broch/quantity[9 "m"] #broch/quantity[3 "s"])
      #broch/quantity[2 "h"] (b// #broch/quantity[4 "Wh"] #broch/quantity[2 "W"])
      #broch/quantity[2 "h"] (b// #broch/quantity[4 "kWh"] (b/* 1000 #broch/quantity[2 "W"]))
      #broch/quantity[2 "J"] (b/* #broch/quantity[1 "N"] #broch/quantity[2 "m"])
      #broch/quantity[0 "m"] (b// 0 #broch/quantity[2 "m"]))))

(defn- is-NaN?
  "Test if this number is nan"
  [x] (false? (= x x)))                                     ;; Nan is the only value for which equality is false

(defn- op-equal? [op n m]
  (let [a (b/meters n) b (b/meters m)
        x (op n m)
        y (b/num ((symbol "b" (name op)) a b))]
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
  (is (not (b/quantity? 123)))
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

; TODO (/ #broch/quantity[12 "J"] #broch/quantity[1 "km"])

(comment
  (clojure.test/run-tests))

(ns broch.numbers-test
  (:require [broch.numbers :refer [bigint abs add sub mul div neg upcast downcast]]
            [clojure.test :refer [deftest is run-tests testing]]))

(deftest js-rationalize-test
  (is (= 1.5 (downcast (upcast 1.5))))
  (is (= 1760.9144 (downcast (upcast 1760.9144))))
  (is (= 0.012 (downcast (upcast 0.012))))
  (is (= 1 (downcast (upcast 1))))
  (is (= (div 1 100) 0.01)))


(deftest arithmetic
  (testing "abs"
    (is (= 1 (abs -1)))
    (is (= (bigint 1) (abs (bigint -1)))))

  (testing "doubles"
    (is (= (+ 0.01 0.002) (add 0.01 0.002)))
    (is (= (- 0.002 0.01) (sub 0.002 0.01)))
    (is (= (* 0.01 0.002) (mul 0.01 0.002)))
    (is (= (/ 0.01 0.002) (div 0.01 0.002)))
    (is (= (- 3) (neg 3)))
    (is (= 1.23E-4 (mul 123 1.0E-6))))

  (testing "ints"
    (is (= (+ 1 2) (add 1 2)))
    (is (= (- 1 2) (sub 1 2)))
    (is (= (* 1 2) (mul 1 2)))
    (is (= (/ 1 2) (div 1 2)))
    (is (= (- 3) (neg 3))))

  (testing "mix"
    (is (= (+ 1760 0.9144) (add 1760 0.9144)))
    (is (= (- 1760 0.9144) (sub 1760 0.9144)))
    (is (= (* 1760 0.9144) (mul 1760 0.9144)))
    (is (= (div 2200000 1143) (div 1760 0.9144)))
    (is (= (div 5 18) (mul 1000 (div 1 3600))))
    (is (mul 0.001 (div 1 3600)))
    (is (= 1E-7 (div 1 10000000))))

  (testing "strange"
    (is (NaN? (add 0.01 ##NaN)))
    (is (NaN? (sub 0.01 ##NaN)))
    (is (NaN? (mul 0.01 ##NaN)))
    (is (NaN? (div 0.01 ##NaN)))
    (is (= (+ 0.01 ##Inf) (add 0.01 ##Inf)))
    (is (= (- 0.01 ##Inf) (sub 0.01 ##Inf)))
    (is (= (* 0.01 ##Inf) (mul 0.01 ##Inf)))
    (is (= (/ 0.01 ##Inf) (div 0.01 ##Inf))))

  )


(comment
 (run-tests)
 )

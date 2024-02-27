(ns broch.numbers
  (:refer-clojure :exclude [bigint abs ratio? rationalize integer?])
  (:require #?@(:clj  [[clojure.core :as core]]
                :cljs [[cljs.core :as core]
                       [cljs.math :as math]
                       [cljs.pprint]])
            [clojure.string :as str])
  #?(:clj (:import (clojure.lang Ratio))))

(defn bigint [n]
  #?(:clj  (core/bigint n)
     :cljs (js/BigInt n)))
(defn abs [n] (cond-> n (neg? n) (-)))

(defprotocol ExactArithmetic
  (add* [this other])
  (sub* [this other])
  (mul* [this other])
  (div* [this other]))

(declare lowest-equiv-ratio)
(declare do-in-common-ratio)
(declare upcast)
(declare ->JSRatio)
(declare ratio?)

(defn- numer [r] (.-numerator r))
(defn- denom [r] (.-denominator r))


#?(:clj
   (deftype JSRatio [numerator denominator]
     Object
     (toString [_] (str numerator "/" denominator)))
   :cljs
   (deftype JSRatio [numerator denominator]
     Object
     (toString [_] (str numerator "/" denominator))
     (equiv [this other] (-equiv this other))
     IEquiv
     (-equiv [this other]
       (or (and (ratio? other)
                (= (bigint numerator) (bigint (numer other)))
                (= (bigint denominator) (bigint (denom other))))
           (and (or (= js/Number (type other)) (= js/BigInt (type other)))
                (= this (upcast other)))))
     IHash
     (-hash [_] (hash {:numerator (str numerator) :denominator (str denominator)}))
     IComparable
     (-compare [this other] (let [[na da] [(bigint (numer this)) (bigint (denom this))]
                                  [nb db] [(bigint (numer other)) (bigint (denom other))]]
                              (cond
                                (= (* na db) (* nb da)) 0
                                (< (* na db) (* nb da)) -1
                                :else 1)))
     IPrintWithWriter
     (-pr-writer [_ writer _] (-write writer (str numerator "/" denominator)))
     ExactArithmetic
     (add* [this other] (do-in-common-ratio this (upcast other) +))
     (sub* [this other] (do-in-common-ratio this (upcast other) -))
     (mul* [_ other] (let [other-r (upcast other)]
                      (lowest-equiv-ratio
                       (->JSRatio (* (bigint numerator) (numer other-r))
                                  (* (bigint denominator) (denom other-r))))))
     (div* [_ other] (let [other-r (upcast other)]
                      (lowest-equiv-ratio
                       (->JSRatio (* (bigint numerator) (denom other-r))
                                  (* (numer other-r) (bigint denominator))))))))

(defn ratio? [x]
  #?(:clj (core/ratio? x)
     :cljs (instance? JSRatio x)))

#?(:cljs
   (defn- do-in-common-ratio [a b op]
     (let [[na da] [(bigint (numer a)) (bigint (denom a))]
           [nb db] [(bigint (numer b)) (bigint (denom b))]]
       (lowest-equiv-ratio
        (if (= da db)
          (->JSRatio (op na nb) da)
          (->JSRatio (op (* na db) (* nb da))
                     (* da db)))))))

#?(:cljs
   (defn- gcd
     "Greatest common denominator"
     [a b]
     (if (or (= 0 b) (= (bigint 0) b))
       a
       (recur b (mod a b)))))

#?(:cljs
   (defn- lowest-equiv-ratio [r]
     (let [d     (gcd (abs (numer r)) (abs (denom r)))
           num   (/ (numer r) d)
           denom (/ (denom r) d)]
       (if (= denom 1)
         num
         (->JSRatio num denom)))))

(defn integer? [n]
  #?(:clj (core/integer? n)
     :cljs (or (int? n) (= js/BigInt (type n)))))

(defn rationalize [n]
  #?(:clj (core/rationalize n)
     :cljs
     (lowest-equiv-ratio
      (cond
        (ratio? n) (->JSRatio (bigint (numer n)) (bigint (denom n)))
        (integer? n) (->JSRatio (bigint n) (bigint 1))
        (str/includes? (str n) "e") (let [[num exp] (str/split (str n) #"e")
                                          factor (bigint (math/pow 10 (abs (js/Number exp))))]
                                      (mul* (rationalize (js/Number num))
                                            (if (neg? exp)
                                              (->JSRatio (bigint 1) factor)
                                              (->JSRatio factor (bigint 1)))))
        (< (abs n) 1) (->JSRatio (as-> (str n) $
                                       (str/split $ #"\.")
                                       (second $)
                                       (js/Number $)
                                       (cond-> $ (neg? n) (-))
                                       (bigint $))
                                 (as-> (str n) $
                                       (str/split $ #"\.")
                                       (second $)
                                       (count $)
                                       (repeat $ 10)
                                       (reduce * $)
                                       (bigint $)))
        :else (as-> (str n) $
                    (str/split $ #"\.")
                    (second $)
                    (str "0." $)
                    (js/Number $)
                    (cond-> $ (neg? n) (-))
                    (add* (rationalize $) (rationalize (abs (long n)))))))))

(defn upcast
  "Make a number into a ratio."
  [n]
  #?(:clj (if (integer? n)
            (bigint n)
            (rationalize n))
     :cljs (rationalize n)))

#?(:cljs (defn js-ratio->number [^JSRatio r] (/ (js/Number (numer r)) (js/Number (denom r)))))

(defn weird? [n]
  (or (and (number? n) (NaN? n)) (infinite? n)))

(defn downcast
  "Downcast if possible without losing precision."
  [n]
  #?(:clj  (cond-> n
             (and (ratio? n) (= n (upcast (unchecked-double n)))) (double)
             (== n (unchecked-long n)) (long))
     :cljs (cond
             (and (ratio? n) (= n (js-ratio->number n))) (js-ratio->number n)
             (ratio? n) (->JSRatio (downcast (numer n)) (downcast (denom n)))
             (and (integer? n) (= n (bigint (js/Number n)))) (js/Number n)
             :else n)))

(defn- rational-op [a b op weird-op]
  (if (or (weird? a) (weird? b))
    (weird-op a b)
    (downcast (op (upcast a) (upcast b)))))

#?(:clj (when (find-ns 'cljs.compiler)
          (require 'cljs.compiler)
          (eval
            '(defmethod cljs.compiler/emit-constant* JSRatio [r]
               (cljs.compiler/emits "new broch.numbers.JSRatio(")
               (cljs.compiler/emit-constant (numer r))
               (cljs.compiler/emits ",")
               (cljs.compiler/emit-constant (denom r))
               (cljs.compiler/emits ")"))))
   :cljs (defmethod cljs.compiler/emit-constant* JSRatio [r]
           (cljs.compiler/emits "new broch.numbers.JSRatio(")
           (cljs.compiler/emit-constant (numer r))
           (cljs.compiler/emits ",")
           (cljs.compiler/emit-constant (denom r))
           (cljs.compiler/emits ")")))




;; API

(defn add [a b] #?(:clj  (rational-op a b + +)
                   :cljs (rational-op a b add* +)))
(defn sub [a b] #?(:clj  (rational-op a b - -)
                   :cljs (rational-op a b sub* -)))
(defn mul [a b] #?(:clj  (rational-op a b * *)
                   :cljs (rational-op a b mul* *)))
(defn div [a b] #?(:clj  (rational-op a b / /)
                   :cljs (rational-op a b div* /)))
(defn neg [n] #?(:clj  (- n)
                 :cljs (if (ratio? n)
                         (->JSRatio (- (numer n)) (denom n))
                         (- n))))
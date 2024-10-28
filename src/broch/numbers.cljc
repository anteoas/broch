(ns broch.numbers
  (:refer-clojure :exclude [bigint abs ratio? rationalize integer? number?])
  (:require #?@(:clj  [[clojure.core :as core]]
                :cljs [[cljs.core :as core]
                       [cljs.math :as math]
                       [cljs.pprint]
                       [clojure.string :as str]])))

(defprotocol ExactArithmetic
  (add* [this other])
  (sub* [this other])
  (mul* [this other])
  (div* [this other]))

(defn ->bigint [n]
  #?(:clj  (core/bigint n)
     :cljs (js/BigInt n)))

#?(:clj
   (deftype JSRatio [numerator denominator]
     Object
     (toString [_] (str numerator "/" denominator)))
   :cljs
   (deftype JSRatio [numerator denominator]
     Object
     (toString [_] (str numerator "/" denominator))
     (equiv [this other] (-equiv this other))
     IHash
     (-hash [_] (hash {:numerator (str numerator) :denominator (str denominator)}))
     IPrintWithWriter
     (-pr-writer [_ writer _] (-write writer (str numerator "/" denominator)))))

(defn numer [^JSRatio r] (.-numerator r))
(defn denom [^JSRatio r] (.-denominator r))

(defn js-ratio [n m]
  (->JSRatio (->bigint n) (->bigint m)))

(defn ratio? [x]
  #?(:clj  (core/ratio? x)
     :cljs (instance? JSRatio x)))

(defn number? [x]
  (or (clojure.core/number? x) (ratio? x)))

(defn integer? [n]
  #?(:clj  (core/integer? n)
     :cljs (or (int? n) (= js/BigInt (type n)))))

(defn- gcd
  "Greatest common denominator"
  [a b]
  (if (or (= 0 b) (= (->bigint 0) b))
    a
    (recur b (mod a b))))

(defn abs* [n] (cond-> n (neg? n) (-)))

(defn- lowest-equiv-ratio [r]
  (let [d     (gcd (abs* (numer r)) (abs* (denom r)))
        num   (/ (numer r) d)
        denom (/ (denom r) d)]
    (if (= denom 1)
      num
      (js-ratio num denom))))

(defn- do-in-common-ratio [a b op]
  (let [[na da] [(->bigint (numer a)) (->bigint (denom a))]
        [nb db] [(->bigint (numer b)) (->bigint (denom b))]]
    (lowest-equiv-ratio
     (if (= da db)
       (js-ratio (op na nb) da)
       (js-ratio (op (* na db) (* nb da))
                 (* da db))))))

(defn rationalize [n]
  "Make a number into a ratio"
  #?(:clj (core/rationalize n)
     :cljs
     (lowest-equiv-ratio
      (cond
        (ratio? n) n
        (integer? n) (js-ratio n 1)
        (str/includes? (str n) "e") (let [[num exp] (str/split (str n) #"e")
                                          factor (math/pow 10 (abs* (js/Number exp)))]
                                      (mul* (rationalize (js/Number num))
                                            (if (neg? exp)
                                              (js-ratio 1 factor)
                                              (js-ratio factor 1))))
        :else (let [d-str (second (str/split (str n) #"\."))]
                (add* (rationalize (long n))
                      (js-ratio (cond-> (js/Number d-str) (neg? n) (-))
                                (reduce * (repeat (count d-str) 10)))))))))

#?(:cljs
   (extend-type JSRatio
     IEquiv
     (-equiv [this other]
       (cond
         (instance? JSRatio other) (and (= (->bigint (numer this)) (->bigint (numer other)))
                                        (= (->bigint (denom this)) (->bigint (denom other))))
         (or (= js/Number (type other)) (= js/BigInt (type other))) (= this (rationalize other))
         :else false))
     IComparable
     (-compare [this other] (let [other-r (rationalize other)
                                  [na da] [(->bigint (numer this)) (->bigint (denom this))]
                                  [nb db] [(->bigint (numer other-r)) (->bigint (denom other-r))]]
                              (cond
                                (= (* na db) (* nb da)) 0
                                (< (* na db) (* nb da)) -1
                                :else 1)))
     ExactArithmetic
     (add* [this other] (do-in-common-ratio this (rationalize other) +))
     (sub* [this other] (do-in-common-ratio this (rationalize other) -))
     (mul* [this other] (let [other-r (rationalize other)]
                          (lowest-equiv-ratio
                           (js-ratio (* (->bigint (numer this)) (numer other-r))
                                     (* (->bigint (denom this)) (denom other-r))))))
     (div* [this other] (let [other-r (rationalize other)]
                          (lowest-equiv-ratio
                           (js-ratio (* (->bigint (numer this)) (denom other-r))
                                     (* (numer other-r) (->bigint (denom this)))))))))

(defn upcast
  "Upcast to something with support for exact precision."
  [n]
  #?(:clj  (if (integer? n)
             (->bigint n)
             (rationalize n))
     :cljs (rationalize n)))

#?(:cljs (defn js-ratio->number [^JSRatio r]
           (/ (js/Number (numer r)) (js/Number (denom r)))))

(defn downcast
  "Downcast if possible without losing precision."
  [n]
  #?(:clj  (cond-> n
             (and (ratio? n) (= n (upcast (unchecked-double n)))) (double)
             (== n (unchecked-long n)) (long))
     :cljs (cond
             (and (ratio? n) (= n (js-ratio->number n))) (js-ratio->number n)
             (ratio? n) (js-ratio (downcast (numer n)) (downcast (denom n)))
             (and (integer? n) (= n (->bigint (js/Number n)))) (js/Number n)
             :else n)))

(defn weird? [n]
  (or (and (clojure.core/number? n) (NaN? n))               ; how can something be a number and not a number? ask my nan
      (infinite? n)))

(defn- rational-op [a b op weird-op]
  (if (or (weird? a) (weird? b))
    (weird-op a b)
    (downcast (op (upcast a) (upcast b)))))

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
                         (js-ratio (- (numer n)) (denom n))
                         (- n))))

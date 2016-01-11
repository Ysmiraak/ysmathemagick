(ns ysmathemagick.trials
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]))

(defn fib [n]
  (let [r5 (math/sqrt 5)]
    (math/round
     (* (/ 1 r5)
        (- (math/expt (/ (+ 1 r5) 2) n)
           (math/expt (/ (- 1 r5) 2) n))))))

(def fib-corecur
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+' a b)))))
   0 1))

(def fib-iterate
  (as-> (fn [[a b]] [b (+' a b)]) $
    (iterate $ [0 1])
    (map first $)))

(def fib-lazy-seq
  (->> (rest fib-lazy-seq)
       (map +' fib-lazy-seq)
       lazy-seq
       (cons 1)
       (cons 0)))

(def fib-lazy-cat
  (->> (rest fib-lazy-cat)
       (map +' fib-lazy-cat)
       (lazy-cat [0 1])))

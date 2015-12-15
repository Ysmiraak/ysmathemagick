(ns ysmathemagick.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]))

(def r5 (math/sqrt 5))

(defn a [n]
  (math/round
   (* (/ 1 r5)
      (- (math/expt (/ (+ 1 r5) 2) n)
         (math/expt (/ (- 1 r5) 2) n)))))

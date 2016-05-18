(ns ysmathemagick.statistics
  (:require [clojure.core.matrix :as m]
            (clojure.core.matrix
             ;; operators linear dataset selection random
             [stats :as s])
            incanter.stats))

(defmacro table
  "(let [a 1 b 2 c 3] (table a b c)) => {:a 1, :b 2, :c 3}"
  [& symbols]
  `(zipmap (map keyword '~symbols) (list ~@symbols)))

(defn count-choose
  "Returns the number of n choose k."
  [n k]
  (reduce / (reduce * (range (inc (- n k)) (inc n)))
          (range 1 (inc k))))

(defn measures "
  Returns the precision, recall, accuracy, and f1-score measures
  based on the number of true positives, true negative, false
  positives, and false negatives.

  (measures 7 1 9 3) => 
  {:precision 7/16, :recall 7/10, :accuracy 2/5, :f1-score 7/13}

  (measures 1 7 3 9) => 
  {:precision 1/4, :recall 1/10, :accuracy 2/5, :f1-score 1/7}
  " [tp tn fp fn]
  (let [precision (/ tp (+ tp fp))
        recall    (/ tp (+ tp fn))
        accuracy  (/ (+ tp tn)
                     (+ tp tn fp fn))
        f1-score  (/ (* 2 precision recall)
                     (+ precision recall))]
    (table precision recall accuracy f1-score)))

(defn Z-test "
  Test the hypothesis that the sample comes from the normmally distributed population.

  (Z-test [100 105 104 105 107 110 99 111 106 105] 100 13.04) =>
  {:reject false, :p 0.20729768969012463, :Z 1.2610309687788017, :Cohen-s_d 0.39877300613496935}
  
  (Z-test (repeat 100 105.2) 100 13.04) =>
  {:reject true, :p 6.670850363388325E-5, :Z 3.9877300613497506, :Cohen-s_d 0.398773006134975}
  " [sample population-mean population-sd
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? false}}]
  (let [sample-mean (s/mean sample)
        SDM (/ population-sd (m/sqrt (count sample)))
        Z (/ (- sample-mean population-mean) SDM)
        p (* (if two-sided? 1 2) (- 1 (incanter.stats/cdf-normal (m/abs Z))))
        reject (< p alpha)
        Cohen-s_d (/ (- sample-mean population-mean) population-sd)]
    (table reject p Z Cohen-s_d)))

(defn- t->p [t DF two-sides?]
  (* (if two-sides? 1 2)
     (- 1 (incanter.stats/cdf-t (m/abs t) :df DF))))

(defn t-test
  "Test the hypothesis that the sample comes from the normmally distributed population.

  (t-test [100 105 104 105 107 110 99 111 106 105] 100) =>
  {:reject true, :p 0.0018045343966888172, :t 4.367161585455664, :DF 9}
  " [sample population-mean
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? false}}]
  (let [[sample-size sample-mean sample-sd]
        (map #(% sample) [count s/mean s/sd])
        DF (dec sample-size)
        SEM (/ sample-sd (m/sqrt sample-size))
        t (/ (- sample-mean population-mean) SEM)
        p (t->p t DF two-sided?)
        reject (< p alpha)]
    (table reject p t DF)))



(defn paired-t-test "
  Test the hypothesis that there is no difference between the two
  measurements of a sample from a normally distributed population.

  (paired-t-test (map -
                      [100 105 104 105 107 110 99 111 106 105]
                      [106 115 106 112 110 115 108 116 110 120]))
  =>
  {:reject true, :p 4.325011389971767E-4, :t -5.400892783340812, :DF 9}
  " [sample-diff
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? false}}]
  (let [[sample-size diff-mean diff-sd]
        (map #(% sample-diff) [count s/mean s/sd])
        DF (dec sample-size)
        SEM (/ diff-sd (m/sqrt sample-size))
        t (/ diff-mean SEM)
        p (t->p t DF two-sided?)
        reject (< p alpha)]
    (table reject p t DF)))

(defn two-sample-t-test "
  Test the hypothesis that there is no difference between the
  population (normally distributed) means of two independent samples.
  
  
  " [sample-1 sample-2
     & {:keys [alpha two-sided? pooled?]
        :or {alpha 0.01 two-sided? false pooled? false}}]
  (let [[size-1 size-2 mean-1 mean-2 var-1 var-2]
        (for [f [count s/mean s/variance]
              s [sample-1 sample-2]] (f s))
        DF (if pooled?
             (- (+ size-1 size-2) 2)
             (/ (m/pow (+ var-1 var-2) 2)
                (+ (/ (m/pow var-1 2) (dec size-1))
                   (/ (m/pow var-2 2) (dec size-2)))))
        t (/ (- mean-1 mean-2) (m/sqrt (+ var-1 var-2)))
        p (t->p t DF two-sided?)
        reject (< p alpha)]
    (table reject p t DF)))

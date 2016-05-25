(ns ysmathemagick.statistics
  (:require
   [clojure.core.matrix :as m]
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

(defn fake-sample
  "Returns a vector of numbers with size (minimally 2), mean, and sd."
  [size mean sd]
  (assert (<= 2 size))
  (let [oddity (mod size 2)
        size (- size oddity)
        deviation (* sd (m/sqrt (/ (+ size -1 oddity) size)))]
    (->> (map #((if (even? %) + -) mean deviation) (range size))
         (concat (repeat oddity mean))
         (into []))))

(defn v->p "
  Calculates the p-value from some other value v in
  distribution ∈ {:t, :norm, :chi-sq}.
  " [distribution v
     & {:keys [DF two-sided?] :or {DF 1 two-sided? true}}]
  (let [CDF (case distribution
              :t incanter.stats/cdf-t
              :norm incanter.stats/cdf-normal
              :chi-sq incanter.stats/cdf-chisq)
        prob (CDF (m/abs v) :df DF)]
    (* (if two-sided? 2 1) (- 1 prob))))

(defn Z-test "
  Test the hypothesis that the sample of 'size and 'mean comes from
  a normal distribution with 'μ and 'σ as parameters.

  (Z-test 10 105 100 13)
  =>
  {:reject false, :p 0.11194282534736288, :Z 1.2162606385262997, :Cohen-s_d 5/13}

  (Z-test 100 105 100 13)
  =>
  {:reject true, :p 5.9993220448917306E-5, :Z 3.846153846153846, :Cohen-s_d 5/13}
  " [size mean μ σ
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? true}}]
  (let [RMSE (/ σ (m/sqrt size))
        Z (/ (- mean μ) RMSE)
        p (v->p :norm Z :two-sided? two-sided?)
        reject (< p alpha)
        Cohen-s_d (/ (- mean μ) σ)]
    (table reject p Z Cohen-s_d)))

(defn t-test "
  Test the hypothesis that the sample of 'size, 'mean, and 'sd comes from
  a normal distribution with 'μ as a parameter.

  (let [population-mean 100
        sample [100 105 104 105 107 110 99 111 106 105]
        [size mean sd] (map #(% sample) [count s/mean s/sd])]
    (t-test size mean sd population-mean))
  =>
  {:reject true, :p 9.022671983444086E-4, :t 4.367161585455664, :DF 9}
  " [size mean sd μ
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? true}}]
  (let [DF (dec size)
        SEM (/ sd (m/sqrt size))
        t (/ (- mean μ) SEM)
        p (v->p :t t :DF DF :two-sided? two-sided?)
        reject (< p alpha)]
    (table reject p t DF)))

  (let [population-mean 100
        sample [100 105 104 105 107 110 99 111 106 105]
        [size mean sd] (map #(% sample) [count s/mean s/sd])]
    (t-test size mean sd population-mean))

(defn paired-t-test "
  Test the hypothesis that there is no difference between the two
  measurements of a sample from a normally distributed population.

  (paired-t-test (map -
                      [100 105 104 105 107 110 99 111 106 105]
                      [106 115 106 112 110 115 108 116 110 120]))
  =>
  {:reject true, :p 2.1625056949858834E-4, :t -5.400892783340812, :DF 9}
  " [sample-diff
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? false}}]
  (let [[sample-size diff-mean diff-sd]
        (map #(% sample-diff) [count s/mean s/sd])
        DF (dec sample-size)
        SEM (/ diff-sd (m/sqrt sample-size))
        t (/ diff-mean SEM)
        p (v->p :t t :DF DF :two-sided? two-sided?)
        reject (< p alpha)]
    (table reject p t DF)))

(defn two-sample-t-test "
  Test the hypothesis that there is no difference between the
  population (normally distributed) means of two independent samples.
  
  (two-sample-t-test (fake-sample 8 190 9) (fake-sample 10 105 13))
  =>
  {:reject true, :p 1.340261235327489E-11, :t 16.350689614778208, :DF 15.747261049437471}
  " [sample-1 sample-2
     & {:keys [alpha two-sided? pooled?]
        :or {alpha 0.01 two-sided? false pooled? false}}]
  (let [variance-of-the-mean #(/ (s/variance %) (count %))
        [size-1 size-2 mean-1 mean-2 mean-var-1 mean-var-2]
        (for [f [count s/mean variance-of-the-mean]
              s [sample-1 sample-2]] (f s))
        DF (if pooled?
             (- (+ size-1 size-2) 2)
             (/ (m/square (+ mean-var-1 mean-var-2))
                (+ (/ (m/square mean-var-1) (dec size-1))
                   (/ (m/square mean-var-2) (dec size-2)))))
        t (/ (- mean-1 mean-2) (m/sqrt (+ mean-var-1 mean-var-2)))
        p (v->p :t t :DF DF :two-sided? two-sided?)
        reject (< p alpha)]
    (table reject p t DF)))

(defn chi-square-test "
  Test the hypothesis that two categorical properties are independent.
  
  (chi-square-test [[25 25][15 35]] :alpha 0.05)
  =>
  {:reject true, :p 0.041226833337163815, :chi-sq 25/6, :DF 1}
  " [contingency-table
     & {:keys [alpha two-sided?]
        :or {alpha 0.01}}]
  (assert (apply = (map count contingency-table)))
  (let [rows contingency-table
        cols (apply map vector rows)
        [row-count row-sums col-count col-sums]
        (for [t [rows cols] f [count #(map s/sum %)]] (f t))
        total-sum (s/sum row-sums)
        DF (* (dec row-count) (dec col-count))
        formula (fn [observed expected]
                  (/ (m/square (- observed expected)) expected))
        chi-sq (s/sum (map formula
                           (for [row rows cell row] cell)
                           (for [row-sum row-sums col-sum col-sums]
                             (/ (* row-sum col-sum) total-sum))))
        p (v->p :chi-sq chi-sq :DF DF :two-sided? false)
        reject (< p alpha)]
    (table reject p chi-sq DF)))

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
     & {:keys [df two-sided?] :or {df 1 two-sided? true}}]
  (let [cdf (case distribution
              :t incanter.stats/cdf-t
              :norm incanter.stats/cdf-normal
              :chi-sq incanter.stats/cdf-chisq)
        prob (cdf (m/abs v) :df df)]
    (* (if two-sided? 2 1) (- 1 prob))))

(defn z-test "
  Test the hypothesis that the sample of 'size and 'mean comes from
  a normal distribution with 'μ and 'σ as parameters.

  (z-test 10 105 13 100)
  =>
  {:reject false, :p 0.22388565069472577, :z 1.2162606385262997, :Cohen-s_d 5/13}

  (z-test 100 105 13 100)
  =>
  {:reject true, :p 1.1998644089783461E-4, :z 3.846153846153846, :Cohen-s_d 5/13}
  " [size mean σ μ
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? true}}]
  (let [rmse (/ σ (m/sqrt size))
        z (/ (- mean μ) rmse)
        p (v->p :norm z :two-sided? two-sided?)
        reject (< p alpha)
        Cohen-s_d (/ (- mean μ) σ)]
    (table reject p z Cohen-s_d)))

(defn t-test "
  Test the hypothesis that the sample of 'size, 'mean, and 'sd comes from
  a normal distribution with 'μ as a parameter.

  (let [population-mean 100
        sample [100 105 104 105 107 110 99 111 106 105]
        [size mean sd] (map #(% sample) [count s/mean s/sd])]
    (t-test size mean sd population-mean))
  =>
  {:reject true, :p 0.0018045343966888172, :t 4.367161585455664, :df 9}
  " [size mean sd μ
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? true}}]
  (let [df (dec size)
        sem (/ sd (m/sqrt size))
        t (/ (- mean μ) sem)
        p (v->p :t t :df df :two-sided? two-sided?)
        reject (< p alpha)]
    (table reject p t df)))

(defn paired-t-test "
  Test the hypothesis that there is no difference between the two
  measurements of a sample from a normally distributed population.

  (paired-t-test (map -
                      [100 105 104 105 107 110 99 111 106 105]
                      [106 115 106 112 110 115 108 116 110 120]))
  =>
  {:reject true, :p 2.1625056949858834E-4, :t -5.400892783340812, :df 9}
  " [sample-diff
     & {:keys [alpha two-sided?] :or {alpha 0.01 two-sided? false}}]
  (let [[sample-size diff-mean diff-sd]
        (map #(% sample-diff) [count s/mean s/sd])
        df (dec sample-size)
        sem (/ diff-sd (m/sqrt sample-size))
        t (/ diff-mean sem)
        p (v->p :t t :df df :two-sided? two-sided?)
        reject (< p alpha)]
    (table reject p t df)))

(defn two-sample-t-test "
  Test the hypothesis that there is no difference between the
  population (normally distributed) means of two independent samples.
  
  (two-sample-t-test (fake-sample 8 190 9) (fake-sample 10 105 13))
  =>
  {:reject true, :p 1.340261235327489E-11, :t 16.350689614778208, :df 15.747261049437471}
  " [sample-1 sample-2
     & {:keys [alpha two-sided? pooled?]
        :or {alpha 0.01 two-sided? false pooled? false}}]
  (let [variance-of-the-mean #(/ (s/variance %) (count %))
        [size-1 size-2 mean-1 mean-2 mean-var-1 mean-var-2]
        (for [f [count s/mean variance-of-the-mean]
              s [sample-1 sample-2]] (f s))
        df (if pooled?
             (- (+ size-1 size-2) 2)
             (/ (m/square (+ mean-var-1 mean-var-2))
                (+ (/ (m/square mean-var-1) (dec size-1))
                   (/ (m/square mean-var-2) (dec size-2)))))
        t (/ (- mean-1 mean-2) (m/sqrt (+ mean-var-1 mean-var-2)))
        p (v->p :t t :df df :two-sided? two-sided?)
        reject (< p alpha)]
    (table reject p t df)))

(defn chi-square-test "
  Test the hypothesis that two categorical variables are independent.
  
  (chi-square-test [[25 25][15 35]] :alpha 0.05)
  =>
  {:reject true, :p 0.041226833337163815, :chi-sq 25/6, :df 1}
  " [contingency-table
     & {:keys [alpha] :or {alpha 0.01}}]
  (assert (apply = (map count contingency-table)))
  (let [rows contingency-table
        cols (apply map vector rows)
        [row-count row-sums col-count col-sums]
        (for [t [rows cols] f [count #(map s/sum %)]] (f t))
        total-sum (s/sum row-sums)
        df (* (dec row-count) (dec col-count))
        formula (fn [observed expected]
                  (/ (m/square (- observed expected)) expected))
        chi-sq (s/sum (map formula
                           (for [row rows cell row] cell)
                           (for [row-sum row-sums col-sum col-sums]
                             (/ (* row-sum col-sum) total-sum))))
        p (v->p :chi-sq chi-sq :df df :two-sided? false)
        reject (< p alpha)]
    (table reject p chi-sq df)))

(defn g-test "
  Test the hypothesis that two categorical variables are independent.
  
  (g-test [[25 25][15 35]] :alpha 0.05)
  =>
  {:reject true, :p 0.04039573850405631, :g 4.201185140367425, :df 1}
  " [contingency-table
     & {:keys [alpha] :or {alpha 0.01}}]
  (assert (apply = (map count contingency-table)))
  (let [rows contingency-table
        cols (apply map vector rows)
        [row-count row-sums col-count col-sums]
        (for [t [rows cols] f [count #(map s/sum %)]] (f t))
        total-sum (s/sum row-sums)
        df (* (dec row-count) (dec col-count))
        formula (fn [observed expected]
                  (* 2 observed (m/log (/ observed expected))))
        g (s/sum (map formula
                      (for [row rows cell row] cell)
                      (for [row-sum row-sums col-sum col-sums]
                        (/ (* row-sum col-sum) total-sum))))
        p (v->p :chi-sq g :df df :two-sided? false)
        reject (< p alpha)]
    (table reject p g df)))

(defn log-likelihood-ratio "
  Alternative method for calculatnig the G-value of two binary
  categorial variables x and y.

  (log-likelihood-ratio 40 50 25 100)
  =>
  4.201185140367414
  " [x y xy n]
  (let [-x (- n x)
        y-x (- y xy)
        py (/ y n)
        py|x (/ xy x)
        py|-x (/ y-x (- n x))]
    (* -2
       (- (+ (* xy (m/log py))
             (* (- x xy) (m/log (- 1 py)))
             (* y-x (m/log py))
             (* (- -x y-x) (m/log (- 1 py))))
          (+ (* xy (m/log py|x))
             (* (- x xy) (m/log (- 1 py|x)))
             (* y-x (m/log py|-x))
             (* (- -x y-x) (m/log (- 1 py|-x))))))))

(defn hidden-markov-prob "
  markov-model: hidden-state -> hidden-state -> probability;
  likelihood-model: hidden-state -> observed-event -> likelihood;

  (def markov-model
    {:start {:hot 0.8
             :cold 0.2}
     :hot {:hot 0.7
           :cold 0.3}
     :cold {:hot 0.4
            :cold 0.6}})
  
  (def likelihood-model
    {:hot {1 0.2
           2 0.4
           3 0.4}
     :cold {1 0.5
            2 0.4
            3 0.1}})
  
  (hidden-markov-prob markov-model likelihood-model [1 1 2])
  =>
  0.033760000000000005
  " [markov-model likelihood-model observation-sequence]
  (let [state+ (keys (markov-model :start))        
        step-f (fn [state==prob o]
                 (for [s' state+]
                   [s' (->> (* p (get-in markov-model [s s']) l)
                            (for [[s p] state==prob])
                            (let [l (get-in likelihood-model [s' o])])
                            (reduce +))]))
        |state+| (count state+)]
    (->> observation-sequence
         (reduce step-f (repeat |state+| [:start (/ |state+|)]))
         (map peek)
         (reduce +))))

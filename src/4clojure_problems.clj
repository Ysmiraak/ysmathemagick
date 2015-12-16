(require '[clojure.math.numeric-tower :as math]
         '[clojure.math.combinatorics :as comb])

(def r5 (math/sqrt 5))

(defn a [n]
  (math/round
   (* (/ 1 r5)
      (- (math/expt (/ (+ 1 r5) 2) n)
         (math/expt (/ (- 1 r5) 2) n)))))

(def fib-lazy-cat
  (lazy-cat [0 1] (map +' fib-lazy-cat (rest fib-lazy-cat))))

(def fib-self-seq
  (cons 0 (cons 1 (lazy-seq (map +' fib-self-seq (rest fib-self-seq))))))

(def fib-lazy-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+' a b)))))
   0 1))

(def fib-iterate
  (#(map first
        (iterate (fn [[a b]] [b (+' a b)])
                 [%1 %2]))
   0 1))

;; parentheses: how to make this efficient enough???
(letfn [(insert [s i] (concat (take i s) [\( \)] (drop i s)))
        (insertall [s]
          (for [i (range (inc (count s)))]
            (insert s i)))]
  (defn par [n]
    (set (map #(apply str %)
              (loop [i 0 s [[]]]
                (if (= i n) s
                    (recur (inc i) (set (mapcat insertall s)))))))))

;; map magic
(defn my-merge-with [f & ms]
  (if (< (count ms) 2)
    (first ms)
    (let [m1 (first ms)
          m2 (second ms)
          mr (drop 2 ms)
          nm (merge m1 (into {}
                             (for [[k v2] m2]
                               (let [v1 (get m1 k)]
                                 (if (nil? v1)
                                   [k v2]
                                   [k (f v1 v2)])))))]
      (apply mmw f nm mr))))

(fn [f x & xs]
  (reduce
   (fn [rm sm]
     (reduce
      (fn [m [k v]]
        (if (contains? m k)
          (assoc m k (f (m k) v))
          (assoc m k v)))
      rm sm))
   x xs))

(fn [f & maps]
  (letfn [(step
            [m [k v]] 
            (if (contains? m k)
              (assoc m k (f (get m k) v))
              (assoc m k v)))]
    (reduce #(reduce step %1 %2) maps)))

(fn
  [f & maps]
  (letfn [(step
            [m [k v]] 
            (if (contains? m k)
              (assoc m k (f (get m k) v))
              (assoc m k v)))]
    (reduce #(reduce step %1 %2) maps)))

(fn [f & l]
  (let [g (group-by first (apply concat (map #(apply list %) l)))]
    (zipmap (keys g) (map #(reduce f (map second %)) (vals g)))))

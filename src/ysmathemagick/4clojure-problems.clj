(defn fac
  ([n] (fac n 1))
  ([n r]
   (if (zero? n)
     r
     (recur (dec n) (*' r n)))))

(defn dfs
  [graph goal]
  (fn search
    [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> current graph keys
             (remove visited)
             (mapcat #(search (conj path %) (conj visited %))))))))

(defn findpath
  [graph start goal]
  ((dfs graph goal) [start] #{start}))

(def graph 
  {:s {:a 3 :d 4}
   :a {:s 3 :d 5 :b 4}
   :b {:a 4 :e 5 :c 4} 
   :c {:b 4} 
   :d {:s 4 :a 5 :e 2} 
   :e {:d 2 :b 5 :f 4} 
   :f {:e 4 :g 1}})

(findpath graph :s :g)



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



(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

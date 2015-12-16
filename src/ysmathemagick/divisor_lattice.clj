(ns ysmathemagick.divisor_lattice)

(defn print-to-dot [m]
  "http://sandbox.kidstrythisathome.com/erdos/"
  (println "digraph g {")
  (doseq [e m v (val e)]
    (println v "->" (key e) \;))
  (println \}))

(defn find-divisors [n]
  (->> (range 1 (inc n))
       (map #(/ n %))
       (filterv #(zero? (mod % 1)))))

(defn find-ordering [vs]
  (letfn [(divisor? [n] (fn [m] (zero? (mod n m))))
          (up-edges [vs v] (if (vector? vs) (conj vs v) [v]))
          (add [v w-vs w]
            (if (not-any? (divisor? v) (w-vs w))
              (dfs (update w-vs w up-edges v) w)
              w-vs))
          (dfs [w-vs v]
            (reduce (partial add v) w-vs
                    (->> vs
                         (drop-while #(>= % v))
                         (filter (divisor? v)))))]
    (dfs {} (first vs))))

(-> 180
    find-divisors
    find-ordering
    print-to-dot)

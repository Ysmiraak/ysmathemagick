(ns ysmathemagick.divisor_lattice
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]))

(defn print-to-dot [s]
  "http://sandbox.kidstrythisathome.com/erdos/"
  (println "digraph g {")
  (doseq [[v w] s]
    (println v "->" w \;))
  (println \}))

(defn find-divisors [n]
  (->> (range 1 (inc n))
       (map #(/ n %))
       (filterv #(= 0 (mod % 1)))))

(defn find-ordering [vertices]
  (letfn
      [(divisor? [n] (fn [m] (= 0 (mod n m))))
       
       (add [v edges w]
         (if (empty? (->> edges
                          (filter #(= w (second %)))
                          (map first)
                          (filter (divisor? v))))
           (dfs (conj edges [v w]) w)
           edges))
       
       (dfs [edges vertex]
         (reduce (partial add vertex)
                 edges
                 (->> vertices
                      (drop-while #(>= % vertex))
                      (filter (divisor? vertex)))))]
    
      (dfs #{} (first vertices))))



(def vertices (find-divisors 90))

(def edges (find-ordering vertices))

(print-to-dot edges)

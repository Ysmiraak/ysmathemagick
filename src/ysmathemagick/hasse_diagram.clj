(ns ysmathemagick.hasse-diagram
  "functions for drawing hasse diagrams when a relation function and the vertices
  are given."
  (:require [clojure.set :as set]))

(defn transitive-reduce
  "performs transitive reduction on `dag`, which must be a map with keys
  as vertices and vals as adjacency sets."
  [dag]
  (reduce-kv
   (fn [dag fro tos]
     (assoc dag fro
            (->> (map dag tos)
                 (remove nil?)
                 (apply set/union) 
                 (set/difference tos))))
   dag dag))

(defn form-hasse-graph
  "returns a dag representing the hasse diagram constructed with vertices from
  `vs` and ordered by `rf` which must be an arity-2 predicate for a relation."
  [vs rf]
  (->> (for [f vs t vs]
         (if (and (not= f t) (rf f t))
           {f #{t}} {}))
       (apply merge-with set/union)
       transitive-reduce))

(defn print-in-dot
  "http://sandbox.kidstrythisathome.com/erdos/"
  [dag]
  (println "digraph g {")
  (doseq [[fro tos] dag, to tos]
    (println (str \" fro \") "->" (str \" to \" \;)))
  (println \}))

(comment
  (letfn [(divisible-by? [n m] (zero? (mod n m)))]
    (-> (filter (partial divisible-by? 90) (range 1 91))
        (form-hasse-graph divisible-by?)
        print-in-dot
        with-out-str))
  => "digraph g {\n\"15\" -> \"3\";\n\"15\" -> \"5\";\n\"90\" -> \"45\";\n\"90\" -> \"30\";\n\"90\" -> \"18\";\n\"6\" -> \"3\";\n\"6\" -> \"2\";\n\"3\" -> \"1\";\n\"2\" -> \"1\";\n\"9\" -> \"3\";\n\"5\" -> \"1\";\n\"45\" -> \"15\";\n\"45\" -> \"9\";\n\"30\" -> \"15\";\n\"30\" -> \"6\";\n\"30\" -> \"10\";\n\"10\" -> \"2\";\n\"10\" -> \"5\";\n\"18\" -> \"6\";\n\"18\" -> \"9\";\n}\n")

(comment
  (-> [#{1 2 3} #{1 2} #{2 3} #{1 3} #{1} #{2} #{3} #{}]
      (form-hasse-graph clojure.set/superset?)
      print-in-dot
      with-out-str)
  => "digraph g {\n\"#{1 3 2}\" -> \"#{1 3}\";\n\"#{1 3 2}\" -> \"#{1 2}\";\n\"#{1 3 2}\" -> \"#{3 2}\";\n\"#{1 2}\" -> \"#{2}\";\n\"#{1 2}\" -> \"#{1}\";\n\"#{3 2}\" -> \"#{3}\";\n\"#{3 2}\" -> \"#{2}\";\n\"#{1 3}\" -> \"#{3}\";\n\"#{1 3}\" -> \"#{1}\";\n\"#{1}\" -> \"#{}\";\n\"#{2}\" -> \"#{}\";\n\"#{3}\" -> \"#{}\";\n}\n")

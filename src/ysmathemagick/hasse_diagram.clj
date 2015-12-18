(ns ysmathemagick.hasse-diagram
  (:use [clojure.set :only [union difference]]))

(defn transitive-reduce
  "Performs transitive reduction on DAG, which must be a map with keys
  as vertices and vals as adjacency sets."
  [DAG]
  (reduce-kv
   (fn [dag fro tos]
     (assoc dag fro
            (->> (map dag tos)
                 (remove nil?)
                 (apply union) 
                 (difference tos))))
   DAG DAG))

(defn form-hasse-graph
  "Returns a DAG representing the Hasse diagram constructed with
  vertices from vs and ordered by rf. The function rf must be a binary
  relation that returns either true or false."
  [vs rf]
  (->> (for [f vs t vs]
         (if (and (not= f t) (rf f t))
           {f #{t}} {}))
       (apply merge-with union)
       transitive-reduce))

(defn print-in-dot
  "http://sandbox.kidstrythisathome.com/erdos/"
  [DAG]
  (println "digraph g {")
  (doseq [[fro tos] DAG to tos]
    (println (str \" fro \") "->" (str \" to \" \;)))
  (println \}))

(defn divisible-by?
  "Returns true if n is divisible by m, and false otherwise."
  [n m]
  (zero? (mod n m)))

(-> (filter (partial divisible-by? 90) (range 1 91))
    (form-hasse-graph divisible-by?)
    print-in-dot)

(-> [#{1 2 3} #{1 2} #{2 3} #{1 3} #{1} #{2} #{3} #{}]
    (form-hasse-graph clojure.set/superset?)
    print-in-dot)

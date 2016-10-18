(ns ysmathemagick.statistics
  "functions for statistical analysis."
  (:require [clojure.spec :as s]
            [clojure.spec
             [gen :as g]
             [test :as t]]))

(s/def ::finum (s/and number? #(Double/isFinite %)))

(s/fdef table :args (s/* symbol?))
(defmacro table
  "`(let [a 1 b 2 c 3] (table a b c)) => {:a 1, :b 2, :c 3}`"
  [& symbols]
  `(zipmap (map keyword '~symbols) (list ~@symbols)))

(s/fdef float->string
        :args (s/cat :f nat-int? :n number?)
        :ret string?
        :fn #(let [{:keys [f n]} (-> % :args)
                   res (-> % :ret Double/parseDouble)]
               (cond
                 (Double/isNaN n) (Double/isNaN res)
                 (Double/isInfinite n) (== n res)
                 :else (<= (Math/abs (- res n)) (reduce * (repeat f 0.1))))))
(defn float->string
  "formats the number `n` with floating precision `f`."
  [f n] (format (str "%." f \f) (double n)))

(s/fdef count-choose
        :args (s/cat :n nat-int? :k nat-int?)
        :ret integer?)
(defn count-choose
  "returns the cardinality of n choose k."
  [n k]
  (reduce / (reduce *' (range (inc (- n k)) (inc n)))
          (range 1 (inc k))))

;; (s/fdef fake-sample
;;         :args (s/cat :size (s/and pos-int? #(<= 2 %))
;;                      :mean ::finum
;;                      :sd (s/and ::finum (comp not neg?)))
;;         :ret vector?
;;         :fn (s/and #(== (-> % :ret count) (-> % :args :size))
;;                    #(< (- (-> % :ret sta/mean) (-> % :args :mean)) 0.0001)
;;                    #(< (- (-> % :ret sta/sd) (-> % :args :sd)) 0.0001)))
;; (defn fake-sample
;;   "returns a vector of numbers with `size` (minimally 2), `mean`, and `sd`."
;;   [size mean sd]
;;   (let [oddity (mod size 2)
;;         size (- size oddity)
;;         deviation (* sd (Math/sqrt (double (/ (+ size -1 oddity) size))))]
;;     (->> (map #((if (even? %) + -) mean deviation) (range size))
;;          (concat (repeat oddity mean))
;;          (into []))))

(defmacro check [f n]
  `(let [f# (resolve '~f), {args$# :args ret$# :ret fn$# :fn} (s/get-spec f#)]
     (if (and args$# ret$# fn$#)
       (loop [[[args# ret#] & next#] (s/exercise-fn f# ~n)]
         (or (s/explain-data fn$# {:args (s/conform args$# args#) :ret (s/conform ret$# ret#)})
             (if next# (recur next#) :clojure.spec/passed)))
       :clojure.spec/missing)))

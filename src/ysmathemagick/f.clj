(ns ysmathemagick.f)

(defprotocol Functor (map1 [F f]))

(defrecord F [a b])

(extend-protocol Functor
  nil               (map1 [_ _] nil)
  F                 (map1 [F f] (update F :b f))
  clojure.lang.ISeq (map1 [[x & xs] f] (cons x (lazy-seq (f xs)))))

;;;;;;;;;;;;;;;;;;
;; catamorphism ;;
;;;;;;;;;;;;;;;;;;

(defn f
  "F a = Nil | F a b"
  ([a] (fn [b] (F. a b)))
  ([a b] (F. a b)))

(defn fix
  "Fix (F a) = (F a (Fix (F a)))"
  [fa] (fn ([] fa) ([b] #(fa b))))

(defn unfix
  "unfix (Fix (F a)) = F a"
  [f] (f))

(defn cata
  "cata alg = alg . fmap (cata alg) . unFix"
  [alg]
  (fn [arg] (alg (map1 (unfix arg) (cata alg)))))

(defn len [f]
  (if-let [{b :b} f]
    (inc b)
    0))

(defn sum [f]
  (if-let [{a :a b :b} f]
    (+ a b)
    0))

(defn fib [f]
  (if-let [{[m n] :b} f]
    [n (+ m n)]
    [0 1]))

(comment
  ((cata len) (->> nil fix (f 1) fix (f 2) fix (f 3) fix (f 4) fix))
  => 4
  ((cata sum) (->> nil fix (f 1) fix (f 2) fix (f 3) fix (f 4) fix))
  => 10
  ((cata fib) (->> nil fix (f 1) fix (f 2) fix (f 3) fix (f 4) fix))
  => [3 5]
  )

;;;;;;;;;;;;;;;;;
;; anamorphism ;;
;;;;;;;;;;;;;;;;;

(def fix identity)

(defn ana [coalg]
  (fn [arg] (fix (map1 (coalg arg) (ana coalg)))))

(defn era
  "sieve of eratosthenes."
  [[p & ns]]
  (cons p (remove #(zero? (rem % p)) ns)))

(comment

  ((ana era) (drop 2 (range)))

  (def prime
    ((fn ana [[p & ns]] (lazy-seq (cons p (ana (remove (fn [n] (zero? (rem n p))) ns)))))
     (drop 2 (range))))

  (->> (range)
       (drop 2)
       (iterate (fn [[p & ns]] (remove #(zero? (rem % p)) ns)))
       (map first))

  (def fib (lazy-cat [0 1] (map + fib (next fib))))

  )

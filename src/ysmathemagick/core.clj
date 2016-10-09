(ns ysmathemagick.core "my study notes on lambda calculus.")

(defmacro λ
  "lambda expression"
  [var & exp] `(fn [~var] ~@exp))

(def lexicon
  "decodes Church encoding"
  (atom {}))

(defn β
  "beta reduction (with decoding)"
  [f & fs]
  (let [r (reduce (fn [a b] (a b)) f fs)]
    (@lexicon r r)))

;;;;;;;;;;;;;;;;;;;;;
;; Church booleans ;;
;;;;;;;;;;;;;;;;;;;;;

(def T "λx.λy.x" (λ x (λ y x))) ;; x is always true, no matter what y
(def F "λx.λy.y" (λ x (λ y y))) ;; if x is true, ex falso
(def IF  "λc.λa.λb.c a b" (λ c (λ a (λ b ((c a) b)))))
(def NOT "λc.λa.λb.c b a" (λ c (λ a (λ b ((c b) a)))))
(swap! lexicon assoc T :true F :false IF :if NOT :not)

(comment
  (β IF  T T F)
  => :true
  (β IF  F T F)
  => :false
  (β NOT T T F)
  => :false
  (β NOT F T F)
  => :true)

;; note: clojure 'if is not defined this way
(comment
  (if true (/ 1 1) (/ 1 0))
  => 1)
(comment
  (β IF T (/ 1 1) (/ 1 0))
  => "ArithmeticException Divide by zero")
;; because clojure evals in applicative order and not normal order

;;;;;;;;;;;;;;;;;;;;;;;
;; logical operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; one of many possible formulations
(def AND "λp.λq.p q p" (λ p (λ q ((p q) p))))
(def OR  "λp.λq.p p q" (λ p (λ q ((p p) q))))
(swap! lexicon assoc AND :and OR :or)

(comment
  (β AND T T)
  => :true
  (β AND T F)
  => :false
  (β AND F T)
  => :false
  (β AND F F)
  => :false
  (β OR  T T)
  => :true
  (β OR  T F)
  => :true
  (β OR  F T)
  => :true
  (β OR  F F)
  => :false)

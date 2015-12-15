(ns ysmathemagick.core)

(defmacro λ
  "lambda expression"
  [f & args]
  `(fn [~f] ~@args))

(def lexicon
  "decodes Church encoding"
  (atom {}))

(defn β
  "beta reduction (with decoding)"
  [& args]
  (let [r (reduce #(%1 %2) args)]
    (@lexicon r r)))

;;;;;;;;;;;;;;;;;;;;;
;; Church booleans ;;
;;;;;;;;;;;;;;;;;;;;;

(def T "λx.λy.x" (λ x (λ y x))) ;; x is always true, no matter what y
(def F "λx.λy.y" (λ x (λ y y))) ;; if x is true, ex falso
(def IF  "λc.λa.λb.c a b" (λ c (λ a (λ b ((c a) b)))))
(def NOT "λc.λa.λb.c b a" (λ c (λ a (λ b ((c b) a)))))
(swap! lexicon assoc T :true F :false IF :if NOT :not)

(β IF  T T F)
(β IF  F T F)
(β NOT T T F)
(β NOT F T F)

;; NOTE: lisp 'if is not defined this way
(if true (/ 1 1) (/ 1 0)) ;; => 1
;; (β IF T (/ 1 1) (/ 1 0)) ;; => ArithmeticException Divide by zero
;; because lisp used applicative order and not normal order
;; because lisp is a practical language, hence
;; https://en.wikipedia.org/wiki/McCarthy_Formalism

;;;;;;;;;;;;;;;;;;;;;;;
;; logical operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; one of many possible formulations
(def AND "λp.λq.p q p" (λ p (λ q ((p q) p))))
(def OR  "λp.λq.p p q" (λ p (λ q ((p p) q))))
(swap! lexicon assoc AND :and OR :or)

(β AND T T)
(β AND T F)
(β AND F T)
(β AND F F)
(β OR  T T)
(β OR  T F)
(β OR  F T)
(β OR  F F)

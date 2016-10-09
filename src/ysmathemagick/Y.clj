(ns ysmathemagick.Y
  "my study notes on various fixpoint combinators."
  (:use [ysmathemagick.core :only [λ]]))

(defn factorial
  "a typical recursive factorial function"
  [n] (if (zero? n) 1 (* n (factorial (dec n)))))

;; lambda expression, being anonymous, cannot invoke itself

(def fac'
  "but we can pass the function to itself
  and let lambda bind and name itself"
  (λ self
     (let [f (λ n ((self self) n))]
       (λ n (if (zero? n) 1 (* n (f (dec n))))))))

(comment
  (map (fac' fac') (range 7))
  => (1 1 2 6 24 120 720))

;; this kind of self application (f f) can be regularly replaced
;; by the application of a fixpoint combinator y as (y f)

(def fac
  "this non-recursive function can be turned
  into a recursive factorial function
  by a fixpoint combinator y"
  (λ f (λ n (if (zero? n) 1 (* n (f (dec n)))))))

;; since we want (y f) = (f (y f))
;; (defn y [f] (f (y f))) should work, if clojure uses normal order
;; but still, we could have this
(defn υ [f] (f #((υ f) %)))
(comment
  (map (υ fac) (range 7))
  => (1 1 2 6 24 120 720))
;; because (y f) = (λ v ((y f) v))
;; and that the wrapper λv delays evaluation
(defn υ' [f] #((f (υ' f)) %)) ;; works as well
(comment
  (map (υ' fac) (range 7))
  => (1 1 2 6 24 120 720))
;; however, this y is not a combinator since y is unbound

(def ϒ
  "Curry's paradoxical combinator ϒ
  λf.(λx.f (x x)) (λx.f (x x))
  with a wrapper λv for delaying evaluation"
  (λ f ((λ x (f (x x)))
        (λ x (f (λ v ((x x) v)))))))

(comment
  (map (ϒ fac) (range 7))
  => (1 1 2 6 24 120 720))

(def Z
  "the Z combinator, the equivalent form of the ϒ combinator
  in applicative order languages, also works in normal order languages
  λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))"
  (λ f ((λ x (f (λ v ((x x) v))))
        (λ x (f (λ v ((x x) v)))))))

(comment
  (map (Z fac) (range 7))
  => (1 1 2 6 24 120 720))

(def ϒ'
  "the simpler equivalent form of the ϒ combinator
  λf.(λx.x x) (λx.f (x x))
  which β-reduces into the ϒ combinator
  with a wrapper λv for delaying evaluation
  http://mvanier.livejournal.com/2897.html"
  (λ f ((λ x (x x))
        (λ x (f (λ v ((x x) v)))))))

(comment
  (map (ϒ' fac) (range 7))
  => (1 1 2 6 24 120 720))

;; the set of fixed-point combinators of untyped lambda calculus
;; is recursively enumerable (Mayer Goldberg, 2005)

(def Θ
  "Turing's fixed-point combinator
  (λx.λy.(y (x x y))) (λx.λy.(y (x x y)))
  with a wrapper λv for delaying evaluation.
  Special property: (Θ f) ->β (f (Θ f))"
  ((λ x (λ y (y ((x x) y))))
   (λ x (λ y (y (λ v (((x x) y) v)))))))

(comment
  (map (Θ fac) (range 7))
  => (1 1 2 6 24 120 720))

(def Θ'
  "the call-by-value form of the Θ combinator
  works in both normal and applicative order languages
  (λx. λy. (y (λv. x x y v))) (λx. λy. (y (λv. x x y v)))"
  ((λ x (λ y (y (λ v (((x x) y) v)))))
   (λ x (λ y (y (λ v (((x x) y) v)))))))

(comment
  (map (Θ' fac) (range 7))
  => (1 1 2 6 24 120 720))

(def SK-ϒ
  "the simplest fixed point combinator in the SK-calculus
  (λx. λy. x y x) (λy. λx. y (x y x))))
  with a wrapper λv for delaying evaluation"
  ((λ x (λ y ((x y) x)))
   (λ y (λ x (y (λ v (((x y) x) v)))))))

(comment
  (map (SK-ϒ fac) (range 7))
  => (1 1 2 6 24 120 720))

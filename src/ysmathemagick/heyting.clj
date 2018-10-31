(ns heyting)

;; an interval is a pair of natural numbers
;; intv : [ a , b ]
;; inf <= a < sup
;; inf < b <= sup
;; a < b
;; odd a open, even b open
;; even a closed, odd b closed

;; a proposition is a vector of sorted, non-overlapping intervals

(def inf 0)
(def sup 65535)

(def f [])
(def t [[inf sup]])

(def counter (atom inf))

(defn prop! []
  (let [b (swap! counter (partial + 4))]
    ;; [[3 4]] [[7 8]] [[11 12]] ... [[65531 65532]]
    (assert (<= b (- sup 3)))
    ;; all proposition are open
    [[(dec b) b]]))

(defn dual [p]
  (if-let [[[a b] & p] (seq p)]
    (loop [p p ;; remaining intervals
           c b ;; where the previous interval ends
           q (if (zero? a)
               [] ;; special case for the first interval
               [[0 a]])]
      (if-let [[[a b] & p] p]
        ;; from where the previous one ends to where this one starts;
        ;; open boundries becomes closed and closed boundries become
        ;; open due to the switching of left and right
        (recur p b (conj q [c a]))
        ;; special case for the final interval
        (if (< c sup) (conj q [c sup]) q)))
    t))

(defn inte [p]
  (mapv (fn [[a b]]
          ;; 0 1, 2 3, 4 5, ... 65534 65535
          ;; each pair represents one boundry;
          ;; the even one when occurs on the left means closed,
          ;; and when occurs on the right means open;
          ;; the opposite goes for the odd one.
          ;; 0 and 65535 while being closed are still interior,
          ;; therefore require special treatments.
          [(if (= inf a) a (inc (* 2 (quot a 2))))
           (if (= sup b) b (* 2 (quot b 2)))])
        p))

(defn sum
  ([p q]
   (if-let [[[a b] & p] (seq (sort (concat p q)))]
     (loop [a a b b p p q []]
       (if-let [[[c d] & p] p]
         (if (< b c)
           ;; b is the right boundry and
           ;; c is the next left boundry
           (recur c d p (conj q [a b]))
           ;; a is still the left boundry
           ;; (max b d) may be the next right boundry
           ;; c is in the middle
           (recur a (max b d) p q))
         (conj q [a b])))
     []))
  ([p q & rs] (reduce sum (sum p q) rs)))

(defn mul
  ([p q]
   (loop [p (seq p) q (seq q) r []]
     (if (and p q)
       (let [[[a b] & p] p
             [[c d] & q] q]
         (cond
           (<= a b c d) (recur p (cons [c d] q) r)
           (<= a c b d) (recur p (cons [b d] q) (conj r [c b]))
           (<= c a b d) (recur p (cons [b d] q) (conj r [a b]))
           (<= a c d b) (recur (cons [d b] p) q (conj r [c d]))
           (<= c a d b) (recur (cons [d b] p) q (conj r [a d]))
           (<= c d a b) (recur (cons [a b] p) q r)
           :else (/ 0)))
       r)))
  ([p q & rs] (reduce mul (mul p q) rs)))

(defn exp
  ([p q] (inte (sum (dual p) q)))
  ([p q & rs] (reduce exp (exp p q) rs)))

(defn neg [p] (inte (dual p)))

(defn eqv [p q] (mul (exp p q) (exp q p)))

(defn truth?
  ([p] (= t p))
  ([p & qs] (apply = t p qs)))

(comment

  (defn test-inte-dual [p]
    (loop [p p n 6]
      (when-not (zero? n)
        (let [q (dual p)
              r (inte q)]
          (println p "dual" q "inte" r)
          (recur r (dec n))))))

  (def p (prop!))
  (def q (prop!))
  (def r (prop!))
  (def s (prop!))

  ;; monotonicity
  (truth?
   (exp (exp p q)
        (exp (mul p r) q)))
  => true

  ;; disjunction introduction
  (truth?
   (exp p (sum p q))
   (exp q (sum p q)))
  => true

  ;; disjunction elimination
  (truth?
   (exp (mul (sum p q) (exp p r) (exp q r))
        r))
  => true

  ;; disjunctive syllogism
  (truth?
   (eqv (mul (sum p q) (neg p))
        q))
  => true

  ;; hypothetical syllogism
  (truth?
   (exp (mul (exp p q) (exp q r))
        (exp p r)))
  => true

  ;; idempotency
  (truth?
   (eqv (mul p p) p)
   (eqv (sum p p) p)
   (eqv (exp (mul p q q) r)
        (exp (mul p q) r)))
  => true

  ;; contraction
  (truth?
   (exp (exp (mul p q q) r)
        (exp (mul p q) r))
   (exp (exp p (mul q q r))
        (exp p (mul q r))))
  => true

  ;; conjunction elimination
  (truth?
   (exp (mul p q) p)
   (exp (mul p q) q))
  => true

  ;; commutativity of conjunction
  (truth?
   (eqv (mul p q)
        (mul q p)))
  => true

  ;; materal implication
  (truth?
   (eqv (sum (neg p) q)
        (exp p q)))
  => true

  ;; modus ponens
  (truth?
   (exp (mul (exp p q) p)
        q))
  => true

  ;; modus tollens
  (truth?
   (exp (mul (exp p q) (neg q))
        (neg q)))
  => true

  ;; modus ponendo tollens
  (truth?
   (exp (mul (neg (mul p q)) p)
        (neg q)))
  => true

  ;; exportation
  (truth?
   (eqv (exp (mul p q) r)
        (exp p (exp q r))))
  => true

  ;; frege's theorem
  (truth?
   (eqv (exp (exp p q) (exp p r))
        (exp p (exp q r))))
  => true

  ;; biconditional elimination
  (truth?
   (exp (eqv p q)
        (exp p q))
   (exp (eqv p q)
        (exp q p)))
  => true

  ;; transposition
  (truth?
   (eqv (exp (neg q) (neg p))
        (exp p q)))
  => true

  ;; resolution
  (truth?
   (exp (mul (exp p q) (exp (neg p) r))
        (sum q r)))
  => true

  ;; constructive dilemma
  (truth?
   (exp (mul (exp p q) (exp r s) (sum p r))
        (sum q s)))
  => true

  ;; destructive dilemma
  (truth?
   (exp (mul (exp p q) (exp r s) (sum (neg q) (neg s)))
        (sum (neg p) (neg r))))
  => true

  ;; absorption
  (truth?
   (eqv (exp p (mul p q))
        (exp p q)))
  => true

  ;; consequentia mirabilis
  (truth? (exp p f p p))
  => true

  ;; peirce's law
  (truth? (exp p q p p))
  => true

  ;; de morgan's laws
  (truth?
   (eqv (mul (neg p) (neg q))
        (neg (sum p q)))
   (eqv (sum (neg p) (neg q))
        (neg (mul p q))))
  => true

  ;; ex falso
  (truth? (exp f p))
  => true

  ;; double negation
  (truth?
   (eqv p (neg (neg p))))
  => true

  ;; law of identity
  (truth? (eqv p p))
  => true

  ;; law of noncontradiction
  (truth? (neg (mul p (neg p))))
  => true

  ;; law of excluded middle
  (truth?
   (sum p (neg p)))
  => false

  )

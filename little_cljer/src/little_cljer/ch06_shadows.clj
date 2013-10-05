(ns little-cljer.ch06-shadows
  (:require [little-cljer.ch01-toys :refer [atom?]]
            [little-cljer.ch04-numbers-games :refer [+ * exp]])
  (:refer-clojure :exclude [+ *]))


;;; For the purposes of this chapter, `+`, `*` and `exp` are valid arithmetic
;;; operations.
(def op? #{'+ '* 'exp})


;;; An arithmetic expression is of the form:
;;; `(3 + (4 exp 5))`
;;; i.e (aexp op aexp)


(defn numbered?
  "Determine whether the input arithmetic expression is valid.
  To clarify, it is understood that the input is an arithmetic expression
  separated by valid operators, we just have to ensure that the operators
  are operating on valid entities.

  Thus, `(3 + 4)`, `(3 + (4 * 5))` are valid, but `(3 + :bacon)` is not."
  [aexp]
  (cond
   (atom? aexp) (number? aexp)
   :else (and (numbered? (first aexp))
              (numbered? (first (rest (rest aexp)))))))


(defn value
  "Return the value of a numbered arithmetic expression."
  [nexp]
  (cond
   (atom? nexp) nexp

   (= (first (rest nexp)) (quote +))
   (+ (value (first nexp))
      (value (first (rest (rest nexp)))))

   (= (first (rest nexp)) (quote *))
   (* (value (first nexp))
      (value (first (rest (rest nexp)))))

   (= (first (rest nexp)) (quote exp))
   (exp (value (first nexp))
        (value (first (rest (rest nexp)))))))


;;; > The Seventh Commandment
;;; > -----------------------
;;; > Recur on subparts that are of the same nature.


;;; We are now changing the representation of our arithmetic expressions
;;; to prefix notation - ie `(+ 1 2)` is a valid representation.
;;; Note that only two operands are allowed as of now.


(defn first-sub-exp
  "Return the first sub expression of an arithmetic expression."
  [aexp]
  (first (rest aexp)))


(defn second-sub-exp
  "Return the second sub expression of an arithmetic expression."
  [aexp]
  (first (rest (rest aexp))))


(defn operator
  "Return the operator of the arithmetic expression."
  [aexp]
  (first aexp))


(defn value
  "Return the value of a numbered arithmetic expression.
  Remember that we have only 3 valid operators, and that the
  input expression is valid"
  [nexp]
  (cond
   (atom? nexp) nexp

   (= (operator nexp) (quote +))
   (+ (value (first-sub-exp nexp))
      (value (second-sub-exp nexp)))

   (= (operator nexp) (quote *))
   (* (value (first-sub-exp nexp))
      (value (second-sub-exp nexp)))

   :else
   (exp (value (first-sub-exp nexp))
        (value (second-sub-exp nexp)))))


;;; Observe that we can change `first-sub-exp`, `second-sub-exp` and
;;; `operator` functions to deal with any arithmetic representation
;;; (prefix, infix, postfix) and our value function will work just fine.


;;; > The Eighth Commandment
;;; > ----------------------
;;; > Use help functions to abstract from representations.


;;; A new representation for numbers
;;; <code>
;;; 0 = ();
;;; 1 = (());
;;; 2 = (() ());
;;; </code>
;;; and so on.

;;; Let us implement new primitives for this representation.
;;; Recall that zero? inc and dec are the primitives for numbers.


(defn sero?
  "zero?"
  [n]
  (empty? n))


(defn enc
  "inc"
  [n]
  (conj n '()))


(defn zec
  "dec"
  [n]
  (rest n))


(defn plus
  "Re-implementation of +"
  [n m]
  (if (sero? m) n (recur (enc n) (zec m))))


;;; However, will other functions like `lat?` work?
;;; Recall that `(lat? '(1 2 3))` is `true`.
;;; But `(lat? '((()) (() ()) (() () ())))` is very very `false`.


;;; You must beware of **shadows**.

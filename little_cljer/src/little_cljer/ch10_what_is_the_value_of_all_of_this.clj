(ns little-cljer.ch10-what-is-the-value-of-all-of-this
  (:require [little-cljer.ch01-toys :refer [atom?]]
            [little-cljer.ch07-friends-and-relations :refer [build]])
  (:refer-clojure :exclude [apply]))


;;; An entry is a pair of lists whose first list is a set.
;;; Also the two lists must be of equal length
;;; Eg:
;;; `((:a :b :c) (:d :e :f))`
;;; or
;;; `((:a :b :c) (:d :d :d))`

;;; An entry is a representation of a hash-map. The first set is the
;;; keys in the map and the second list is the values of those keys in
;;; the map.

;;; Since an entry is a pair, we can use `build` from ch07 to build an
;;; entry
(def new-entry build)


(defn- lookup-entry-helper
  "Help the `lookup-in-entry` function."
  [name names values entry-f]
  (cond
   (empty? names) (entry-f name)
   (= (first names) name) (first values)
   :else (lookup-entry-helper name (rest names) (rest values) entry-f)))


(defn lookup-in-entry
  "Try to find the value for key `name` in the entry.

  If the key `name` does not exist, invoke the function `entry-f` with
  `name`. Let it provide the value in this case."
  [name entry entry-f]
  (lookup-entry-helper name (first entry) (second entry) entry-f))


;;; A table (also called environment) is a list of entries.
(def extend-table conj)


(defn lookup-in-table
  "Find the value for `name` in `table`. Return the first value you find.

  If the key `name` does not exist, invoke the function `table-f` with
  `name`."
  [name table table-f]
  (if (empty? table)
    (table-f name)
    (lookup-in-entry name
                     (first table)
                     (fn [n] (lookup-in-table n (rest table) table-f)))))


(declare *const *identifier *quote *lambda *cond *application)


(defn atom-to-action
  "Return the action that a given atom represents"
  [e]
  (cond
   (number? e) *const
   (= true e) *const
   (= false e) *const
   (= e (quote conj)) *const
   (= e (quote first)) *const
   (= e (quote rest)) *const
   (= e (quote empty?)) *const
   (= e (quote =)) *const
   (= e (quote atom?)) *const
   (= e (quote zero?)) *const
   (= e (quote inc)) *const
   (= e (quote dec)) *const
   (= e (quote number?)) *const
   :else *identifier))


(defn list-to-action
  "Return the action that a given list represents"
  [e]
  (if (atom? (first e))
    (cond
     (= (first e) (quote quote)) *quote
     (= (first e) (quote fn)) *lambda
     (= (first e) (quote cond)) *cond
     :else *application)
    *application))


(defn expression-to-action
  "Return the action that a given expression represents"
  [e]
  (if (atom? e)
    (atom-to-action e)
    (list-to-action e)))


(defn meaning
  "What is the meaning of all of this?"
  [e table]
  ((expression-to-action e) e table))


(defn value
  "What is the value of all of this?"
  [e]
  (meaning e '()))


;;; Actions do speak louder than words.


(defn *const
  "Numbers and Boolean values represent themselves, all other constant
  types represent primitives"
  [e table]
  (cond
   (number? e) e
   (= e true) true
   (= e false) false
   :else (build (quote :primitive) e)))


(def text-of second)


(defn *quote
  "Just return the `text-of` the quoted expression"
  [e table]
  (text-of e))


(defn initial-table
  "There is no value for the given name."
  [name]
  nil)


(defn *identifier
  "Return the value of the identifier"
  [e table]
  (lookup-in-table e table initial-table))


(defn *lambda
  "Represent the function internally.

  The first component of the representation pair marks it as a
  non-primitive.
  The second component of the pair is a list of the table, formals, and
  body of the non-primitive. It contains all the information necessary
  to evaluate the non-primitive. This is known as a closure record."
  [e table]
  (build (quote :non-primitive)
         (conj (rest e) table)))


(defn third
  "Helper function like the already available `first` and `second`"
  [l]
  (first (rest (rest l))))


(def table-of first)
(def formals-of second)
(def body-of third)


(defn drop-line
  "Helper function when evaluating cond forms.
  Drops the first cond question-answer form."
  [lines]
  (rest (rest lines)))


(def question-of first)
(def answer-of second)


(defn evcon
  "An internal evaluation of the cond form"
  [lines table]
  (cond
   (= :else (question-of lines)) (meaning (answer-of lines) table)
   (meaning (question-of lines) table) (meaning (answer-of lines) table)
   :else (evcon (drop-line lines) table)))


(def cond-lines-of rest)


(defn *cond
  "And now to define cond"
  [e table]
  (evcon (cond-lines-of e) table))


(defn evlis
  "Take a list of args and a table, and return a list composed of the
  meaning of each argument"
  [args table]
  (if (empty? args)
    '()
    (conj (evlis (rest args) table)
          (meaning (first args) table))))


(def function-of first)
(def arguments-of rest)

(defn primitive?
  "Is this a primitive representation?"
  [e]
  (= (first e) :primitive))

(defn non-primitive?
  "Is this a non-primitive representation?"
  [e]
  (= (first e) :non-primitive))

(defn __atom?
  "We need a special atom? function to handle our own representations of
  primitive and non-primitive things."
  [x]
  (cond
   (atom? x) true
   (= (first x) :primitive) true
   (= (first x) :non-primitive) true
   :else false))


(defn apply-primitive
  "Evaluate the arguments on the primitive function"
  [e vals]
  (cond
   (= e (quote conj)) (conj (first vals) (second vals))
   (= e (quote first)) (first vals)
   (= e (quote rest)) (rest vals)
   (= e (quote empty?)) (empty? vals)
   (= e (quote =)) (= (first vals) (second vals))
   (= e (quote atom?)) (__atom? (first vals))
   (= e (quote zero?)) (zero? (first vals))
   (= e (quote inc)) (inc (first vals))
   (= e (quote dec)) (dec (first vals))
   (= e (quote number?)) (number? (first vals))))


(defn apply-closure
  "Evaluate the non-primitive stuff. This is probably the best function
  in this entire book."
  [closure vals]
  (meaning (body-of closure)
           (extend-table (table-of closure)
                         (new-entry (formals-of closure) vals))))


(defn apply
  "Apply a function to the given arguments"
  [func args]
  (cond
   (primitive? func) (apply-primitive (second func) args)
   (non-primitive? func) (apply-closure (second func) args)))


(defn *application
  "Apply the function of the form to it's arguments."
  [e table]
  (apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))


;;; Example code:
;; <code><pre>
;;     (value '((fn [coffee klutz party]
;;                (cond
;;                 coffee klutz
;;                 :else party))
;;              false 1 2))
;; => 2
;; </pre></code>
;;;

;;; Are we finished now? Yes, we are exhausted.


;;; The final piece of the interpreter is define, which is a useful
;;; abstraction unless you decide to use the Y combinator
;;; everywhere. To tackle define, head over to 'The Seasoned Schemer'


;;; It's time for a banquet.

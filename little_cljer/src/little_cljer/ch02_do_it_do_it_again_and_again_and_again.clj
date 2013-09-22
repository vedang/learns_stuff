(ns little-cljer.ch02-do-it-do-it-again-and-again-and-again
  (:require [little-cljer.ch01-toys :refer [atom?]]))

;;; Language primitives that we need to know:

;;; 1. define -> def
;;; Binds a variable to a value

;;; 2. lambda -> fn
;;; Defines a function.

;;; 3. defn ->
;;; (defn ...) is a short form for (def ... (fn))


(defn lat?
  "Return true if `l` is a list of atoms"
  [l]
  (cond
   (empty? l) true
   (atom? (first l)) (lat? (rest l))
   :else false))

;;; And now in idiomatic clojure

(defn lat?
  "Return true if `l` is a list of atoms"
  [l]
  (every? atom? l))


;;; 4. or -> or


(defn member?
  "Return true if atom `a` is a member of the list `lat`"
  [a lat]
  (cond
   (empty? lat) false
   :else (or (= a (first lat))
             (member? a (rest lat)))))

;;; In idiomatic clojure

(defn member?
  "Return true if atom `a` is a member of the list `lat`"
  [a lat]
  (boolean ((set lat) a)))


;;; The First Commandment
;;; =====================
;;; Always ask `null?` (CLJ: empty? / nil?) as the first question in
;;; expressing any function


;;; Do you believe all this? Then you may rest.

;;; This space for doodling

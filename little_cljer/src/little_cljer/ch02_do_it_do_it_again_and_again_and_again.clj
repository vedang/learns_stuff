(ns little-cljer.ch02-do-it-do-it-again-and-again-and-again
  (:require [little-cljer.ch01-toys :refer [atom?]]))

;;; Language primitives that we need to know:

;;; `define` -> `def`

;;; Binds a variable to a value

;;; `lambda` -> `fn`

;;; Defines a function.
;;; `(defn ...)` is a short form for `(def ... (fn))`


(defn lat?
  "Return true if `l` is a list of atoms"
  [l]
  (cond
   (empty? l) true
   (atom? (first l)) (lat? (rest l))
   :else false))


(defn member?
  "Return true if atom `a` is a member of the list `lat`"
  [a lat]
  (cond
   (empty? lat) false
   :else (or (= a (first lat))
             (member? a (rest lat)))))


;;; > The First Commandment
;;; > ---------------------
;;; > Always ask `null?` as the first question in expressing any
;;; > function.


;;; Do you believe all this? Then you may rest.

;;; This space for doodling

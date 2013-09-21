(ns little-cljer.ch01-toys)

;;; This chapter introduces us to the primitives of scheme.
;;; The primitives and their clojure equivalents are as follows:

;;; 1. car -> first

;;; 2. cdr -> rest

;;; 3. cons -> conj
;;; Note: There is a cons in clojure, but it is
;;; considered as internal plumbing and should not be used unless
;;; you know exactly what you are doing

;;; 4. null? -> empty?, nil?
;;; In clojure, the empty list '() is not equal to nil. Hence we have
;;; these two primitives in the place of null?

;;; 5. atom? -> (complement coll?)
;;; In clojure, we don't have a function to return true or false on an
;;; atom, but we have a function to return true or false on a collection.
;;; Thus we can build our own version of atom?
(def atom? (complement coll?))

;;; 6. eq? -> =
;;; While eq? only works for atom, = will work for anything that
;;; implements the hashCode method of the Object interface. In plain
;;; speak, that means that it'll work for any built-in clojure datatype.


;;; Now go make yourself a peanut butter and jelly sandwich.


;;; This space reserved for
;;; JELLY STAINS!

(ns little-cljer.ch05-oh-my-gawd-its-full-of-stars
  (:require [little-cljer.ch01-toys :refer [atom?]]))


(defn rember*
  "Remove atom `a` from all nested lists in `l`"
  [a l]
  (cond
   (empty? l) '()
   (atom? (first l)) (if (= (first l) a)
                       (rember* a (rest l))
                       (conj (rember* a (rest l)) (first l)))
   :else (conj (rember* a (rest l))
               (rember* a (first l)))))


(defn insertR*
  "insert `new` to the right of `old` in all places in `l`"
  [new old l]
  (cond
   (empty? l) '()
   (atom? (first l)) (if (= (first l) old)
                       (conj (conj (insertR* new old (rest l)) new)
                             old)
                       (conj (insertR* new old (rest l)) (first l)))
   :else (conj (insertR* new old (rest l))
               (insertR* new old (first l)))))


(defn occur*
  "Count the number of occurrences of atom `a` in list `l`"
  [a l]
  (cond
   (empty? l) 0
   (atom? (first l)) (if (= (first l) a)
                       (inc (occur* a (rest l)))
                       (occur* a (rest l)))
   :else (+ (occur* a (first l)) (occur* a (rest l)))))


(defn subst*
  "Substitute all occurrences of `old` with `new` in `l`"
  [new old l]
  (cond
   (empty? l) '()
   (atom? (first l)) (if (= (first l) old)
                       (conj (subst* new old (rest l)) new)
                       (conj (subst* new old (rest l)) (first l)))
   :else (conj (subst* new old (rest l))
               (subst* new old (first l)))))


(defn insertL*
  "insert `new` to the left of `old` in all places in `l`"
  [new old l]
  (cond
   (empty? l) '()
   (atom? (first l)) (if (= (first l) old)
                       (conj (conj (insertL* new old (rest l)) old)
                             new)
                       (conj (insertL* new old (rest l)) (first l)))
   :else (conj (insertL* new old (rest l))
               (insertL* new old (first l)))))


(defn member*
  "Return true if `a` is a member of `l`"
  [a l]
  (cond
   (empty? l) false
   (atom? (first l)) (or (= (first l) a)
                         (member* a (rest l)))
   :else (or (member* a (first l))
             (member* a (rest l)))))


(defn leftmost
  "Find the leftmost atom in `l`"
  [l]
  (if (atom? (first l)) (first l) (recur (first l))))


(defn eqlist?
  "Return true if the two lists are equal"
  [l1 l2]
  (cond
   (and (empty? l1) (empty? l2)) true

   (or (empty? l1) (empty? l2)) false

   (and (atom? (first l1))
        (atom? (first l2))
        (= l1 l2))
   (eqlist? (rest l1) (rest l2))

   (or (atom? (first l1)) (atom? (first l2))) false

   :else (and (eqlist? (first l1) (first l2))
              (eqlist? (rest l1) (rest l2)))))


(defn equal?
  "Return true if two s-expressions are equal"
  [s1 s2]
  (cond
   (and (atom? s1) (atom? s2)) (= s1 s2)
   (or (atom? s1) (atom? s2)) false
   :else (eqlist? s1 s2)))


(defn eqlist?
  "Return true if two lists are equal"
  [l1 l2]
  (cond
   (and (empty? l1) (empty? l2)) true
   (or (empty? l1) (empty? l2)) false
   :else (and (equal? (first l1) (first l2))
              (eqlist? (rest l1) (rest l2)))))


;;; At this point, ^ my mind was blown a little.


;;; The Sixth Commandment
;;; =====================
;;; Simplify only after the function is correct.


(defn rember
  "Define rember to work with any s-expression `s` and any list `l`"
  [s l]
  (cond
   (empty? l) '()
   (equal? (first l) s) (rember s (rest l))
   :else (conj (rember s (rest l)) (first l))))

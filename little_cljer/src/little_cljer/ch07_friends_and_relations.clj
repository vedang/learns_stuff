(ns little-cljer.ch07-friends-and-relations
  (:require [little-cljer.ch02-do-it-do-it-again-and-again-and-again
             :refer [member?]]
            [little-cljer.ch03-cons-the-magnificent :refer [multirember firsts]]
            [little-cljer.ch04-numbers-games :refer [length]]
            [little-cljer.ch01-toys :refer [atom?]])
  (:refer-clojure :exclude [set?]))


(defn set?
  "Return true if `lat` is a set"
  [lat]
  (cond
   (empty? lat) true
   (member? (first lat) (rest lat)) false
   :else (set? (rest lat))))


(defn makeset
  "Given a `lat`, return a set"
  [lat]
  (cond
   (empty? lat) '()
   (member? (first lat) (rest lat)) (makeset (rest lat))
   :else (conj (makeset (rest lat)) (first lat))))


(defn makeset
  "Another implementation of `makeset` using `multirember`"
  [lat]
  (if (empty? lat)
    '()
    (conj (makeset (multirember (first lat) (rest lat)))
          (first lat))))


(defn subset?
  "Return true if `s1` is a subset of `s2`"
  [s1 s2]
  (if (empty? s1)
    true
    (and (member? (first s1) s2)
         (subset? (rest s1) s2))))

(defn eqset?
  "Return true if `s1` and `s2` are equal sets"
  [s1 s2]
  (cond
   (and (empty? s1) (empty? s2)) true
   (or (empty? s1) (empty? s2)) false
   :else (and (member? (first s1) s2)
              (eqset? (rest s1) (multirember (first s1) s2)))))


(defn eqset?
  "Use friends and relations"
  [s1 s2]
  (and (subset? s1 s2)
       (subset? s2 s1)))


(defn intersect?
  "Return true if `s1` and `s2` have at least one common element."
  [s1 s2]
  (if (empty? s1)
    false
   (or (member? (first s1) s2)
       (intersect? (rest s1) s2))))


(defn intersect
  "Return the common elements in `s1` and `s2`"
  [s1 s2]
  (cond
   (empty? s1) '()
   (member? (first s1) s2) (conj (intersect (rest s1) s2) (first s1))
   :else (intersect (rest s1) s2)))


(defn union
  "Return the union of `s1` and `s2`"
  [s1 s2]
  (cond
   (empty? s1) s2
   (member? (first s1) s2) (union (rest s1) s2)
   :else (conj (union (rest s1) s2) (first s1))))


(defn set-difference
  "Return the set-difference of `s1` and `s2`. i.e Return all elements
  of `s1` that are not in `s2`"
  [s1 s2]
  (cond
   (empty? s1) '()
   (member? (first s1) s2) (set-difference (rest s1) s2)
   :else (conj (set-difference (rest s1) s2) (first s1))))


(defn intersectall
  "Return the intersection of all the sets in the list `l`"
  [l]
  (if (empty? (rest l))
    (first l)
    (intersect (first l) (intersectall (rest l)))))


(defn a-pair?
  "Return true if `x` is a pair, i.e, a list with 2 s-expressions"
  [x]
  (if (atom? x)
    false
    (= 2 (length x))))


;;; Clojure already has `first` and `second` as defined at this point.


(defn build
  "Given 2 s-expressions, build a pair."
  [s1 s2]
  (conj (conj '() s2) s1))


;;; A relation (`rel`) is a set of pairs


(defn fun?
  "Return true is (firsts `rel`) is a set"
  [rel]
  (set? (firsts rel)))


(defn revrel
  "Given a `rel`, reverse it."
  [rel]
  (if (empty? rel)
    '()
    (conj (revrel (rest rel))
          (build (first (first rel)) (second (first rel))))))


(defn revpair
  "Given a pair, reverse it."
  [p]
  (build (second p) (first p)))


(defn revrel
  "Friends make things easier to read"
  [rel]
  (if (empty? rel)
    '()
    (conj (revrel (rest rel)) (revpair (first rel)))))


(defn seconds
  "Like `firsts`."
  [ll]
  (if (empty? ll)
    '()
    (conj (seconds (rest ll)) (second (first ll)))))


(defn fullfun?
  "Return true if (seconds `rel`) is a set."
  [rel]
  (set? (seconds rel)))


;;; Another name for `fullfun?` is `one-to-one?` as in 1-1 mapping.
;;; Another definition is as follows


(defn one-to-one?
  "Just like fullfun?"
  [rel]
  (fun? (revrel rel)))


;;; Go get a `((chocolate chip) (doughy cookie))`

;;; Excellent chocolate chip doughy cookie recipe follows.
;;; Not reproduced due to lack of culinary skills.

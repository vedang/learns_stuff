(ns little-cljer.ch04-numbers-games
  (:refer-clojure :exclude [+ - * / > < =]))


(defn +
  "Add two numbers. Use the primitives `zero?` `inc` and `dec`"
  [n m]
  (if (zero? n) m (recur (dec n) (inc m))))


(defn -
  "Sub two numbers. Use the primitives `zero?` `inc` and `dec`"
  [n m]
  (if (zero? m) n (recur (dec n) (dec m))))


(defn addtup
  "Return the sum of all the numbers in a tuple"
  [tup]
  (if (empty? tup) 0 (+ (first tup) (addtup (rest tup)))))


(defn *
  "Return the product of two numbers"
  [n m]
  (if (zero? m) 0 (+ n (* n (dec m)))))


(defn tup+
  "Given 2 tuples, add each element of `tup1` to the
  corresponding element of `tup2`."
  [tup1 tup2]
  (cond
   (empty? tup1) tup2
   (empty? tup2) tup1
   :else (conj (tup+ (rest tup1) (rest tup2))
               (+ (first tup1) (first tup2)))))


(defn >
  "Return if `n` > `m` is true or false"
  [n m]
  (cond
   (zero? n) false
   (zero? m) true
   :else (recur (dec n) (dec m))))


(defn <
  "Return if `n` < `m` is true or false"
  [n m]
  (cond
   (zero? m) false
   (zero? n) true
   :else (recur (dec n) (dec m))))


(defn =
  "Return true if `n` = `m`"
  [n m]
  (cond
   (zero? n) (zero? m)
   (zero? m) false
   :else (recur (dec n) (dec m))))


(defn =
  "Return true if `n` = `m`"
  [n m]
  (cond
   (> n m) false
   (< n m) false
   :else true))


(defn exp
  "Return `n` ^ `m`"
  [n m]
  (if (zero? m) 1 (* n (exp n (dec m)))))


(defn /
  "Return `n` / `m`"
  [n m]
  (if (< n m) 0 (inc (/ (- n m) m))))



;;; Wouldn't a (ham and cheese on rye) be good right now?

;;; Don't forget the mustard!


(defn length
  "Calculate the length of a list"
  [lat]
  (if (empty? lat) 0 (inc (length (rest lat)))))


(defn pick
  "Pick the nth element of list"
  [n lat]
  (if (zero? (dec n)) (first lat) (recur (dec n) (rest lat))))


(defn rempick
  "Remove the nth element from list"
  [n lat]
  (if (zero? (dec n))
    (rest lat)
    (conj (rempick (dec n) (rest lat))
          (first lat))))


(defn no-nums
  "Remove all numbers from a list of atoms"
  [lat]
  (cond
   (empty? lat) '()
   (number? (first lat)) (no-nums (rest lat))
   :else (conj (no-nums (rest lat)) (first lat))))


(defn all-nums
  "Extract a tuple from a `lat`."
  [lat]
  (cond
   (empty? lat) '()
   (number? (first lat)) (conj (all-nums (rest lat)) (first lat))
   :else (all-nums (rest lat))))



(def ^{:doc "An `eq?` function for both numbers and atoms. `eqan?` is
            unnecessary in clojure since `=` will work perfectly fine."}
  eqan? clojure.core/=)


(defn occur
  "Count the number of times `a` occurs in `lat`"
  [a lat]
  (cond
   (empty? lat) 0
   (eqan? a (first lat)) (inc (occur a (rest lat)))
   :else (occur a (rest lat))))


(defn one?
  "Return true if `n` = 1"
  [n]
  (= n 1))

(ns little-cljer.ch08-lambda-the-ultimate
  (:require [little-cljer.ch01-toys :refer [atom?]]
            [little-cljer.ch04-numbers-games :refer [+ * exp]]
            [little-cljer.ch06-shadows
             :refer [operator first-sub-exp second-sub-exp]])
  (:refer-clojure :exclude [+ *]))


(defn rember-f
  "Remove `a` from list `l` according to predicate `test?`"
  [test? a l]
  (cond
   (empty? l) '()
   (test? a (first l)) (rember-f test? a (rest l))
   :else (conj (rember-f test? a (rest l)) (first l))))


(defn eq-c?
  "Return a function to test if input to the function is equal to `a`"
  [a]
  (fn [x]
    (= a x)))


(def eq-salad? (eq-c? :salad))


(defn rember-f
  "Return a function to remove member from list."
  [test?]
  (fn [a l]
    (cond
     (empty? l) '()
     (test? a (first l)) (recur a (rest l))
     :else (conj ((rember-f test?) a (rest l)) (first l)))))


(defn insertL-f
  "The f variant of `insertL` from chapter 3"
  [test?]
  (fn [new old l]
    (cond
     (empty? l) '()
     (test? old (first l)) (conj (conj ((insertL-f test?) new old (rest l))
                                       old)
                                 new)
     :else (conj ((insertL-f test?) new old (rest l)) (first l)))))


(defn insertR-f
  "The f variant of `insertR` from chapter 3"
  [test?]
  (fn [new old l]
    (cond
     (empty? l) '()
     (test? old (first l)) (conj (conj ((insertR-f test?) new old (rest l))
                                       new)
                                 old)
     :else (conj ((insertR-f test?) new old (rest l)) (first l)))))


(defn seqL
  "Helper function: Given `x` `y` and `l`, return [x y ..contents of l..]"
  [x y l]
  (conj (conj l y) x))


(defn seqR
  "Helper function: Given `x` `y` and `l`, return [y x ..contents of l..]"
  [x y l]
  (conj (conj l x) y))


(defn insert-g
  "Return a function which takes 3 args, `new`, `old` and `l`, and
  inserts new next to old in l according to the provided `seqorder`
  function."
  [seqorder]
  (fn [new old l]
    (cond
     (empty? l) '()
     (= old (first l)) (seqorder new old ((insert-g seqorder) new old (rest l)))
     :else (conj ((insert-g seqorder) new old (rest l)) (first l)))))

(def insertL (insert-g seqL))
(def insertR (insert-g seqR))
(def subst (insert-g (fn [x y l] (conj l x))))


;;; ^ This is cool.


;;; > The Ninth Commandment
;;; > ---------------------
;;; > Abstract common patterns with a new function


;;; The function `value` from Chapter 06: Shadows could benefit from a
;;; little abstraction


(defn atom-to-function
  "Given an atom that represents an arithmetic operator, return the
  operator function."
  [atom]
  (cond
   (= atom (quote +)) +
   (= atom (quote *)) *
   :else exp))


(defn value
  "Again, but with our friends and relations."
  [nexp]
  (if (atom? nexp)
    nexp
    ((atom-to-function (operator nexp)) (value (first-sub-exp nexp))
                                        (value (second-sub-exp nexp)))))


;;; An apple a day keeps the doctor away.


;;; Coming back to our `f` functions, let's try `multirember` from
;;; Chapter 03: Cons the Magnificent


(defn multirember-f
  "Return a function which takes `a` and `l` and removes all members of `l`
  that pass `(test? a l)`"
  [test?]
  (fn [a l]
    (cond
     (empty? l) '()
     (test? a (first l)) ((multirember-f test?) a (rest l))
     :else (conj ((multirember-f test?) a (rest l)) (first l)))))

(def multirember (multirember-f =))


;;; What if we only wanted to all instances of `:tuna`. i.e. if our
;;; `a` was fixed, we could combine it with `test?` instead of passing
;;; it separately everywhere.


(defn eq-tuna?
  "Is input = to :tuna?"
  [a]
  (= a :tuna))


(defn multiremberT
  "Remove all elements from `l` that satisfy `test?`"
  [test? l]
  (cond
   (empty? l) '()
   (test? (first l)) (multiremberT test? (rest l))
   :else (conj (multiremberT test? (rest l)) (first l))))


;;; With me so far?
;;; Alright, hold on to your seats. We are now at page 137 and the ride
;;; is going to get a _whole_ lot bumpier.


(defn multirember&co
  "Do many things at one time.

  `col` here is a function, often known as a _collector_
   or a _continuation_"
  [a lat col]
  (cond
   (empty? lat) (col '() '())

   (= a (first lat))
   (multirember&co a (rest lat) (fn [newlat seen]
                                  (col newlat (conj seen (first lat)))))

   :else
   (multirember&co a (rest lat) (fn [newlat seen]
                                  (col (conj newlat (first lat)) seen)))))


;;; Example execution code:
;; <code>
;;    (multirember&co :b
;;                    [:a :b :c :tuna :Tuna :tuna]
;;                    (fn [ls1 ls2]
;;                      [(count ls1) (count ls2)]))
;; </code>
;;;


;;; > The Tenth Commandment
;;; > --------------------
;;; > Build functions to collect more than one value at a time.


(defn multiinsertLR
  "Insert `new` to the left of `oldL` and to the right of `oldR` in `l`"
  [new oldL oldR l]
  (cond
   (empty? l) '()
   (= (first l) oldL) (conj (conj (multiinsertLR new oldL oldR (rest l)) oldL)
                            new)
   (= (first l) oldR) (conj (conj (multiinsertLR new oldL oldR (rest l)) new)
                            oldR)
   :else (conj (multiinsertLR new oldL oldR (rest l)) (first l))))


(defn multiinsertLR&co
  "Multiinsert to the left and right, and also use the number of left and
  right insertions to do other stuff."
  [new oldL oldR l col]
  (cond
   (empty? l) (col '() 0 0)

   (= (first l) oldL)
   (multiinsertLR&co new
                     oldL
                     oldR
                     (rest l)
                     (fn [newlat L R]
                       (col (conj (conj newlat oldL) new)
                            (inc L)
                            R)))

   (= (first l) oldR)
   (multiinsertLR&co new
                     oldL
                     oldR
                     (rest l)
                     (fn [newlat L R]
                       (col (conj (conj newlat new) oldR)
                            L
                            (inc R))))

   :else (multiinsertLR&co new
                           oldL
                           oldR
                           (rest l)
                           (fn [newlat L R]
                             (col (conj newlat (first l)) L R)))))


;;; Remember * functions from
;;; Chapter 05: Oh My Gawd, It's full of Stars!?
;;; Yup, here they come back.


(defn evens-only*
  "This function removes all odd numbers in a nested `l`"
  [l]
  (cond
   (empty? l) '()
   (atom? (first l)) (if (even? (first l))
                       (conj (evens-only* (rest l)) (first l))
                       (evens-only* (rest l)))
   :else (conj (evens-only* (rest l)) (evens-only* (first l)))))


(defn evens-only*&co
  "Remove all the odd numbers in nested `l`.
  Simultaneously, calculate the sum of the even numbers and the product
  of the odd numbers"
  [l col]
  (cond
   (empty? l) (col '() 0 1)

   (atom? (first l)) (if (even? (first l))
                       (evens-only*&co (rest l)
                                       (fn [newl S P]
                                         (col (conj newl (first l))
                                              (+ S (first l))
                                              P)))
                       (evens-only*&co (rest l)
                                       (fn [newl S P]
                                         (col newl
                                              S
                                              (* P (first l))))))

   :else (evens-only*&co (rest l)
                         (fn [newl S P]
                           (evens-only*&co (first l)
                                           (fn [nl s p]
                                             (col (conj newl nl)
                                                  (+ S s)
                                                  (* P p))))))))


;;; Example execution code:
;; <code>
;;      (evens-only*&co [[1 3 5 7] [2 4 6 8] [3 4 5 6] 4 5 6]
;;                      (fn [ls s p]
;;                        [ls s p]))
;; </code>
;;;


;;; Whew!
;;; Is your brain twisted up now?
;;; Go eat a pretzel; don't forget the mustard.

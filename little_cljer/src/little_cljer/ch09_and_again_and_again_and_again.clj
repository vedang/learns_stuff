(ns little-cljer.ch09-and-again-and-again-and-again
  (:require [little-cljer.ch01-toys :refer [atom?]]
            [little-cljer.ch04-numbers-games :refer [pick]]
            [little-cljer.ch07-friends-and-relations
             :refer [build a-pair? revpair]])
  (:refer-clojure :exclude [shuffle]))


(defn keep-looking
  "Keep looking at a lat until you find `a`"
  [a e lat]
  (if (= a e)
    true
    (and (number? e)
         (keep-looking a (pick e lat) lat))))


(defn looking
  "Look at a `lat` and find `a`"
  [a lat]
  (keep-looking a (pick 1 lat) lat))


;;; `looking` can easily go into an endless loop.
;;; Such recursion is called unnatural recursion and such functions
;;; are called partial functions. (Not to be confused with
;;; partially-applied functions in FP). Functions which are known to
;;; always yield a value are called total functions.


(defn shift
  "Shift elements in a pair whose first component is also a pair."
  [pair]
  (build (first (first pair))
         (build (second (first pair)) (second pair))))


(defn align
  "Take a pair or atom and align it according to the defined rules."
  [pora]
  (cond
   (atom? pora) pora
   (a-pair? (first pora)) (align (shift pora))
   :else (build (first pora) (align (second pora)))))


;;; Since `shift` creates an entirely new argument in the recursion
;;; step, the behavior of this function is not predictable. It breaks
;;; the Seventh Commandment: recur on subparts that are of the same
;;; nature.


(defn length*
  "Count the number of atoms in `align`'s arguments."
  [pora]
  (if (atom? pora)
    1
    (+ (length* (first pora)) (length* (second pora)))))


(defn weight*
  "`length*` is not a good measure for `align` because the first
  argument to align is more important than the second argument. We
  should weigh it more."
  [pora]
  (if (atom? pora)
    1
    (+ (* (weight* (first pora)) 2)
       (weight* (second pora)))))


;;; The weight* of align's successive arguments keeps getting smaller.
;;; This measure indicates that every recursion in align works on a
;;; smaller input. Thus align is not a partial function. This means
;;; that it will always yield a value for every argument.


(defn shuffle
  "Maybe like `align`. does things to pairs or atoms."
  [pora]
  (cond
   (atom? pora) pora
   (a-pair? (first pora)) (shuffle (revpair pora))
   :else (build (first pora) (shuffle (second pora)))))


;;; Shuffle is not total, because there is no guarantee that the size
;;; of the input reduces on every recursive call. For example, the
;;; pair ((:a :b) (:c :d)) will recur endlessly.


(defn eternity
  "Never gonna give you up."
  [x]
  (eternity x))


(defn collatz-fn
  "Doesn't yield for 0. No one knows totality for rest of it's arguments.
  Thank you, Lothar Collatz (1910-1990)"
  [n]
  (cond
   (zero? (dec n)) 1
   (even? n) (collatz-fn (/ n 2))
   :else (collatz-fn (inc (* 3 n)))))


(defn ackermann-fn
  "Thank you, Wilhelm Ackermann (1853-1946)."
  [n m]
  (cond
   (zero? n) (inc m)
   (zero? m) (ackermann-fn (dec n) 1)
   :else (ackermann-fn (dec n) (ackermann-fn n (dec m)))))


;;; Ackermann's function is proven to be total. And yet, don't try to
;;; calculate (A 4 3). The bits on this page will rot away long before
;;; your calculation is complete.


;;; You are now on page 157. You should read pages 157-159 for a
;;; concise discussion on the Halting problem that blows my mind
;;; everytime I read it. Go ahead, I'll wait for you.


;;; Thank you Alan Turing (1912-1954) and Kurt Godel (1906-1978)


;;; What if we didn't have `defn`? What if we only had anonymous
;;; functions? How would we recur on these functions? Here is the
;;; function length:


(defn length
  [l]
  (if (empty? l)
    0
    (inc (length (rest l)))))


;;; Now we don't have `defn`, so let's write an anonymous function
;;; that calculates the length of the empty list. For any other kind
;;; of list, this functions gets stuck and returns no answer


(fn [l]
  (if (empty? l)
    0
    (inc (eternity (rest l)))))


;;; Let's try to write a function that calculates the length of lists
;;; with 1 or less items. Just like above.


(fn [l]
  (if (empty? l)
    0
    (inc ((fn [l] (if (empty? l) 0 (inc (eternity (rest l))))) (rest l)))))


;;; Similarly for lists with 2 items


(fn [l]
  (if (empty? l)
    0
    (inc ((fn [l]
            (if (empty? l)
              0
              (inc ((fn [l] (if (empty? l)
                             0
                             (inc (eternity (rest l)))))
                    (rest l)))))
          (rest l)))))


;;; Let's follow the ninth commandment and abstract out this function
;;; that looks like length.

((fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l))))))
 eternity)


;;; ^ This will create out length0 function to give us the length of
;;; an empty list.
;;; We can rewrite our length<=1 function in this style as well.


((fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l))))))
 ((fn [length]
    (fn [l]
      (if (empty? l) 0 (inc (length (rest l))))))
  eternity))


;;; Similarly for length<=2


((fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l))))))
 ((fn [length]
    (fn [l]
      (if (empty? l) 0 (inc (length (rest l))))))
  ((fn [length]
     (fn [l]
       (if (empty? l) 0 (inc (length (rest l))))))
   eternity)))


;;; Now let's try and remove the repetitions from this version of our
;;; code. Let's name the function that takes length as an argument and
;;; returns a function that looks like length. Here is our length0 function:


((fn [mk-length]
   (mk-length eternity))
 (fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l)))))))


;;; Similarly, length<=1 and length<=2


((fn [mk-length]
   (mk-length (mk-length eternity)))
 (fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l)))))))


((fn [mk-length]
   (mk-length (mk-length (mk-length eternity))))
 (fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l)))))))


;;; So, to calculate length-infinity, we need infinite applications of
;;; mk-length i.e, we could have a function to count the length of a
;;; given list, if we knew the length beforehand :P. Well, the
;;; function eternity puts our function into a never-ending tail
;;; spin. So, if you think about it, when we get to the point of
;;; invoking eternity, we don't really care about the function that we
;;; are calling anymore. What would happen if we just pass it
;;; `mk-length` at this point? Our length0 would like this:


((fn [mk-length]
   (mk-length mk-length))
 (fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l)))))))


;;; Just to fuck with your mind a little bit more, we could rewrite it
;;; like this and it would still be the same thing:


((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (if (empty? l) 0 (inc (mk-length (rest l)))))))


;;; Because it's just a name, and names don't matter too much.


;;; Since 1 application of mk-length will give us the length<=1
;;; function, we can apply it as follows:


((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (if (empty? l) 0 (inc ((mk-length eternity) (rest l)))))))


;;; We apply eternity to mk-length, just as it is about to expire, to
;;; extend it for one more use. If we just apply mk-length to
;;; mk-length, if will keep adding recursive uses everytime it is
;;; about to expire.


((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (if (empty? l) 0 (inc ((mk-length mk-length) (rest l)))))))


;;; What is this? This is `length`.

;;; [KABOOM!](http://i.imgur.com/CUz4owd.gif)

;;; Let's add one final touch. `(mk-length mk-length)` is what "looks
;;; like" length. (Refer to the original definition of length.) So
;;; let's re-write our function as follows:


((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   ((fn [length]
      (fn [l]
        (if (empty? l) 0 (inc (length (rest l))))))
    (fn [x]
      ((mk-length mk-length) x)))))


;;; Note how we've wrapped the call to `(mk-length mk-length)` in a
;;; function. Why did we do this? Because if we try to evaluate the
;;; value of `(mk-length mk-length)`, we'll end up in an endless loop.

;;; The function in the center looks exactly like our length
;;; function. Can we extract it out of the innards of `mk-length`?


((fn [le]
   ((fn [mk-length]
      (mk-length mk-length))
    (fn [mk-length]
      (le (fn [x]
            ((mk-length mk-length) x))))))
 (fn [length]
   (fn [l]
     (if (empty? l) 0 (inc (length (rest l)))))))


;;; If the lower function, the one which looks like `length`, is the
;;; length function - then what is the upper function? It's the
;;; function that _MAKES LENGTH_.

;;; This is called the applicative-order Y combinator

(def Y (fn [le]
         ((fn [f] (f f))
          (fn [f] (le (fn [x]
                       ((f f) x)))))))

;;; Here is an example of using the Y combinator to make a function
;;; that sums up numbers from 0 to n

(Y (fn [sum]
     (fn [n]
       (if (zero? n) 0 (+ n (sum (dec n)))))))


;;; What is (Y Y)? Who knows, but it works very hard.
;;; Does your hat still fit? Perhaps not after such a mind stretcher.

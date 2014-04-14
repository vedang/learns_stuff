(ns challenges.spiral)
;;; Issued by: Kapil
;;; Date: <2014-04-14 Mon>
;;; Print input matrix in spiral format
;;; Eg:
;;; Input: 1  2  3  4  5
;;;        6  7  8  9  10
;;;        11 12 13 14 15
;;; Output: 1 2 3 4 5 10 15 14 13 12 11 6 7 8 9


(defn- print-vec
  "Print a vector. Nothing special here."
  [v]
  (when (seq v)
    (print (apply str (interleave v (repeat " "))))))


(declare turn)


(defn barf
  "Print one arm of the spiral"
  [spiral]
  (print-vec (first spiral))
  (when (seq (rest spiral))
    (print-vec (map last (rest spiral)))
    (turn (map butlast (rest spiral)))))


(defn turn
  "Turn the spiral around and bring the next arm to focus"
  [spiral]
  (barf (map reverse (reverse spiral))))


;;; Eg run:
;; challenges.spiral> (barf [[1  2  3  4  5]
;;                           [6  7  8  9  10]
;;                           [11 12 13 14 15]
;;                           [16 17 18 19 20]])
;; 1 2 3 4 5 10 15 20 19 18 17 16 11 6 7 8 9 14 13 12

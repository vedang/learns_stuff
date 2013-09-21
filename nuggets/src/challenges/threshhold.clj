(ns challenges.threshhold)
;;; Issued by: Sidhant
;;; Date: <2013-02-02 Sat>
;;; Running-totals

;; Calculate the running total of a sequence of numbers until it crosses
;; a given threshhold. Return the value that breaks the threshhold. Be
;; lazy about it.

(defn threshhold
  "Return the val that breaks the threshhold"
  ([n vals]
     (threshhold n (first vals) (rest vals)))
  ([n v vals]
     (cond
       (nil? v) nil
       (> 0 (- n (second v))) (first v)
       :else (threshhold (- n (second v)) (first vals) (rest vals)))))


(defn threshhold2
  "Do the same with reductions"
  [n vals]
  (some (fn [[v k]]
          (when (> v n) k))
        (reductions (fn [[s lst] [k v]]
                      [(+ s v) k])
                    [0 '()]
                    vals)))

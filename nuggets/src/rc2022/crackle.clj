(ns rc2022.crackle
  "Write a program that prints out the numbers `start` to
  `end` (inclusive). If the number is divisible by 3, print Crackle
  instead of the number. If it is divisible by 5, print Pop. If it is
  divisible by both 3 and 5, print CracklePop."
  (:require [clojure.test :as t]))

(defn divisible-by?
  "Given a number `num` and a divisor `div`, return true if the number
  is divisible by the divisor."
  [div num]
  (zero? (mod num div)))

(defn crackle-pop
  "Given a number `num`, return Crackle if it is divisible by `div1`, Pop if
  it is divisible by `div2` and CracklePop if it is divisible by both."
  [div1 div2 num]
  (condp divisible-by? num
    (* div1 div2) "CracklePop"
    div1 "Crackle"
    div2 "Pop"
    num))

(comment ;; Solution (collects ans in a list)
  (map (partial crackle-pop 3 5) (range 1 101))
  ;; OR (in case we want to print to STDOUT and not collect the output)
  (doseq [i (range 1 101)]
    (println (crackle-pop 3 5 i))))

(t/deftest divisible-by?-tests
  (t/is (divisible-by? 3 9))
  (t/is (not (divisible-by? 3 10)))
  (t/is (divisible-by? 2 10)))

(t/deftest crackle-pop-tests
  (t/is (= 1 (crackle-pop 2 3 1)))
  (t/is (= "Crackle" (crackle-pop 3 5 3)))
  (t/is (= 4 (crackle-pop 3 5 4)))
  (t/is (= "Pop" (crackle-pop 3 5 5)))
  (t/is (= "CracklePop" (crackle-pop 3 5 15))))

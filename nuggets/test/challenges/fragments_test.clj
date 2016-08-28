(ns challenges.fragments-test
  (:require [challenges.fragments :as cf]
            [clojure.test :as t]))


(t/deftest leading-fragment
  (t/is (= (cf/stitch "my name is "
                      "name is ved"
                      "vedang manerikar")
           "my name is vedang manerikar"))
  (t/is (= (cf/stitch "hi, how do"
                      "how do you "
                      "you do?")
           "hi, how do you do?")))

(t/deftest trailing-fragment
  (t/is (= (cf/stitch "vedang manerikar"
                      "name is ved"
                      "my name is ")
           "my name is vedang manerikar"))
  (t/is (= (cf/stitch "you do?"
                      "how do you "
                      "hi, how do")
           "hi, how do you do?")))

(t/deftest intermingled-fragment
  (t/is (= (cf/stitch "vedang manerikar"
                      "ang manerikar."
                      "my name is ved")
           "my name is vedang manerikar."))
  (t/is (= (cf/stitch "you do?"
                      "hi, how do you"
                      "how do you ")
           "hi, how do you do?")))

(t/deftest overlapping-fragment
  (t/is (= (cf/stitch "vedang manerikar"
                      "ang maneri"
                      "my name is ved")
           "my name is vedang manerikar"))
  (t/is (= (cf/stitch "hi, how do you do?"
                      "hi, how do you"
                      "how do you ")
           "hi, how do you do?")))

(t/deftest unrelated-fragment
  (t/is (or (= (cf/stitch "vedang manerikar"
                          "my name is ")
               "my name is vedang manerikar")
            (= (cf/stitch "vedang manerikar"
                          "my name is ")
               "vedang manerikarmy name is "))))

(t/deftest mixed-fragment
  (t/is (= (cf/stitch "do you know,"
                      "know, koalas like"
                      "you know, koalas like beer?")
           "do you know, koalas like beer?")))

(t/deftest shortest-valid-fragment
  (t/is (= (cf/stitch "he sells sea "
                      "ea shore"
                      "hells by "
                      "a shell"
                      "by the sea sho"
                      "Sh")
           "She sells sea shells by the sea shore")))

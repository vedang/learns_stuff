(ns game-of-life-test
  (:require [game-of-life :as sut]
            [clojure.test :as t]))

(t/deftest test-calculate-neighbour-coordinates
  (t/is (= (sut/calculate-neighbour-coordinates [0 0])
           #{[0 1] [1 0] [1 1]}))
  (t/is (= (sut/calculate-neighbour-coordinates [0 2])
           #{[0 1] [0 3] [1 1] [1 2] [1 3]}))
  (t/is (= (sut/calculate-neighbour-coordinates [1 1])
           #{[0 0] [0 1] [0 2]
             [1 0] [1 2]
             [2 0] [2 1] [2 2]})))

(t/deftest test-find-revived-cells
  (t/is (= (sut/find-revived-cells #{[0 2] [1 0] [1 2] [2 0] [2 1] [2 2]}
                                   #{[0 0] [0 1] [1 1]})
           #{[1 0]})))

(t/deftest test-find-surviving-cells
  (t/is (= (sut/find-surviving-cells #{[0 0] [0 1] [1 1]})
           #{[0 0] [0 1] [1 1]})))

(t/deftest test-calculate-next-generation
  (t/is (= (sut/calculate-next-generation #{[0 0] [0 1] [1 1]})
           #{[0 0] [0 1] [1 0] [1 1]}))
  (t/is (= (sut/calculate-next-generation #{[0 0] [0 1] [1 2]})
           #{[1 1] [0 1]})))

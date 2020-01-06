(ns game-of-life-test
  (:require [game-of-life :as sut]
            [clojure.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as tct]))

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

(defn sum-coords
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn mult-coords
  [[x1 y1] multiplier]
  [(* x1 multiplier) (* y1 multiplier)])

(tct/defspec proptest-calculate-neighbor-coordinates
  ;; Who are your neighbors on the positive side of a 2D grid?
  100
  (prop/for-all
   [coords (gen/vector gen/nat 2)]
   (let [neighbors (sut/calculate-neighbour-coordinates coords)]
     (and
      ;; All neighbors will be positive
      (every? (fn [[x y]]
                (and (<= 0 x)
                     (<= 0 y)))
              neighbors)
      (cond
        (< (count neighbors) 8)
        true

        (= (count neighbors) 8)
        ;; Sum of diagonals will be equal and double of central cell
        (let [sorted-neighbor-cells (sort neighbors)
              corner-1 (first sorted-neighbor-cells)
              center-row-1 (second sorted-neighbor-cells)
              corner-2 (first (drop 2 sorted-neighbor-cells))
              first-row-2 (first (drop 3 sorted-neighbor-cells))
              last-row-2 (first (drop 4 sorted-neighbor-cells))
              corner-3 (first (drop 5 sorted-neighbor-cells))
              center-row-3 (first (drop 6 sorted-neighbor-cells))
              corner-4 (last sorted-neighbor-cells)]
          (= (sum-coords corner-1 corner-4)
             (sum-coords corner-2 corner-3)
             (sum-coords center-row-1 center-row-3)
             (sum-coords first-row-2 last-row-2)
             (mult-coords coords 2)))

        (> (count neighbors) 8)
        false)))))

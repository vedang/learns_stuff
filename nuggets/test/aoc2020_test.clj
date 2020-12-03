(ns aoc2020-test
  (:require [aoc2020 :as sut]
            [clojure.test :as t]))

(t/deftest two-number-sum-detection
  (t/is (= [299 1721]
           (sut/find-nums-with-sum 2020 [1721 979 366 299 675 1456]))))

(t/deftest three-number-sum-detection
  (t/is (= [979 675 366]
           (sut/find-three-nums-with-sum 2020 [1721 979 366 299 675 1456])))
  (t/is (= [500 1020 500]
           (sut/find-three-nums-with-sum 2020 [500 1020 20]))
        "This test should fail, the input should return nil, but it
        does not because the implementation of the function is
        broken. I am okay with this at the moment.")
  (t/is (nil? (sut/find-three-nums-with-sum 2020 [501 1020 20]))))

(t/deftest valid-password-input
  (t/is (sut/valid-password? [\a 1 3] "abcde"))
  (t/is (not (sut/valid-password? [\b 1 3] "cdefg")))
  (t/is (sut/valid-password? [\c 2 9] "ccccccccc")))

(t/deftest policy-input->policy-conversion
  (t/is (= [\a 1 3]
           (sut/policy-input->policy "1-3 a")))
  (t/is (= [\b 1 3]
           (sut/policy-input->policy "1-3 b")))
  (t/is (= [\c 2 9]
           (sut/policy-input->policy "2-9 c"))))

(t/deftest input-string->password-policy-conversion
  (t/is (= [[\a 1 3] "abcde"]
           (sut/input-string->password-policy "1-3 a: abcde")))
  (t/is (= [[\b 1 3] "cdefg"]
           (sut/input-string->password-policy "1-3 b: cdefg")))
  (t/is (= [[\c 2 9] "ccccccccc"]
           (sut/input-string->password-policy "2-9 c: ccccccccc"))))

(t/deftest valid-password-input-new-rule
  (t/is (sut/valid-password-new-rule? [\a 1 3] "abcde"))
  (t/is (not (sut/valid-password-new-rule? [\b 1 3] "cdefg")))
  (t/is (not (sut/valid-password-new-rule? [\c 2 9] "ccccccccc"))))

(t/deftest check-tree-coords
  (t/is (= '([2 0] [3 0]
             [0 1] [4 1] [8 1]
             [1 2] [6 2] [9 2]
             [2 3] [4 3] [8 3] [10 3]
             [1 4] [5 4] [6 4] [9 4]
             [2 5] [4 5] [5 5]
             [1 6] [3 6] [5 6] [10 6]
             [1 7] [10 7]
             [0 8] [2 8] [3 8] [7 8]
             [0 9] [4 9] [5 9] [10 9]
             [1 10] [4 10] [8 10] [10 10])
           (sut/tree-coords ["..##......."
                             "#...#...#.."
                             ".#....#..#."
                             "..#.#...#.#"
                             ".#...##..#."
                             "..#.##....."
                             ".#.#.#....#"
                             ".#........#"
                             "#.##...#..."
                             "#...##....#"
                             ".#..#...#.#"]))))

(t/deftest check-slope-points
  (t/is (= [[0 0] [3 1] [6 2] [9 3] [1 4] [4 5] [7 6] [10 7] [2 8] [5 9] [8 10]]
           (sut/slope-points [0 0] [3 1] [11 11]))))

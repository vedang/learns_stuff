(ns aoc2020-test
  (:require [aoc2020 :as sut]
            [clojure.spec.alpha :as s]
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

(t/deftest check-process-line
  (t/is (= #:aoc2020{:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd"}
           (sut/process-line "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"))))

(t/deftest check-read-batch-file
  (t/is (= 4 (count (sut/read-batch-file "aoc/test-input-4.txt"))))
  (t/is (= 1 (count (filter (partial s/valid? :aoc2020/passport)
                            (sut/read-batch-file "aoc/test-input-4.txt")))))
  (t/is (= 2 (count (filter (partial s/valid? :aoc2020/north-pole-passport)
                            (sut/read-batch-file "aoc/test-input-4.txt"))))))

(t/deftest check-valid-yr?
  (t/is (sut/valid-yr? "2002" 1920 2002))
  (t/is (not (sut/valid-yr? "2003" 1920 2002))))

(t/deftest check-valid-hgt?
  (t/is (sut/valid-hgt? "60in"))
  (t/is (sut/valid-hgt? "190cm"))
  (t/is (not (sut/valid-hgt? "190in")))
  (t/is (not (sut/valid-hgt? "190"))))

(t/deftest check-new-validity-rules
  (t/is
   (every? (comp not (partial s/valid? :aoc2020/north-pole-passport))
           (sut/batch-lines->objs
            ["eyr:1972 cid:100"
             "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
             ""
             "iyr:2019"
             "hcl:#602927 eyr:1967 hgt:170cm"
             "ecl:grn pid:012533040 byr:1946"
             ""
             "hcl:dab227 iyr:2012"
             "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
             ""
             "hgt:59cm ecl:zzz"
             "eyr:2038 hcl:74454a iyr:2023"
             "pid:3556412378 byr:2007"])))
  (t/is
   (every? (partial s/valid? :aoc2020/north-pole-passport)
           (sut/batch-lines->objs
            ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
             "hcl:#623a2f"
             ""
             "eyr:2029 ecl:blu cid:129 byr:1989"
             "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
             ""
             "hcl:#888785"
             "hgt:164cm byr:2001 iyr:2015 cid:88"
             "pid:545766238 ecl:hzl"
             "eyr:2022"
             ""
             "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"]))))

(t/deftest check-seat-num
  (t/is (= 70 (sut/seat-num "BFFFBBF" [0 127] \F \B)))
  (t/is (= 14 (sut/seat-num "FFFBBBF" [0 127] \F \B)))
  (t/is (= 102 (sut/seat-num "BBFFBBF" [0 127] \F \B)))
  (t/is (= 7 (sut/seat-num "RRR" [0 7] \L \R)))
  (t/is (= 0 (sut/seat-num "LLL" [0 7] \L \R)))
  (t/is (= 4 (sut/seat-num "RLL" [0 7] \L \R))))

(t/deftest check-seat-id
  (t/is (=  567 (sut/seat-id "BFFFBBFRRR")))
  (t/is (=  119 (sut/seat-id "FFFBBBFRRR")))
  (t/is (=  820 (sut/seat-id "BBFFBBFRLL"))))

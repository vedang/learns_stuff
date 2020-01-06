(ns oncall-test
  (:require [oncall :as sut]
            [clojure.test :as t]))

(def test-rotation
  "The rotation is expected to be sorted in time (on
  prev-rotation-week value). Since weeks wrap over from 52 to 1, we
  don't want to sort ourselves. We expect the input to take care of
  this.

  Re: duplicate entries. This list can contain duplicate entries (for
  simple book-keeping). However, the code only considers the latest
  entry for any person (unique on name)."
  [{:name "Pardeep"
    :in-next-rotation? false
    :prev-rotation-week 32
    :leaves []}
   {:name "Pavitra"
    :in-next-rotation? false
    :prev-rotation-week 33
    :leaves []}
   {:name "Amogh"
    :in-next-rotation? true
    :prev-rotation-week 34
    :leaves []}
   {:name "Daniel"
    :in-next-rotation? true
    :prev-rotation-week 35
    :assist "Shantanu"
    :leaves []}
   {:name "Mangal"
    :in-next-rotation? true
    :prev-rotation-week 36
    :assist "Shalaka"
    :leaves []}
   {:name "Setia"
    :in-next-rotation? true
    :prev-rotation-week 37
    :assist "Mourjo"
    :leaves []}
   {:name "Shantanu"
    :in-next-rotation? true
    :prev-rotation-week 38
    :leaves []}
   {:name "Suvrat"
    :in-next-rotation? true
    :prev-rotation-week 39
    :leaves []}
   {:name "Somya"
    :in-next-rotation? true
    :prev-rotation-week 40
    :leaves []}
   {:name "Shalaka"
    :in-next-rotation? true
    :prev-rotation-week 41
    :leaves []}
   {:name "Mourjo"
    :in-next-rotation? true
    :prev-rotation-week 42
    :leaves []}
   {:name "Vedang"
    :in-next-rotation? false
    :prev-rotation-week 43
    :leaves [[1 :soft]]}
   {:name "Neha"
    :in-next-rotation? true
    :prev-rotation-week 44
    :leaves []}
   {:name "Faiz"
    :in-next-rotation? true
    :prev-rotation-week 45
    :leaves []}
   {:name "Harsh"
    :in-next-rotation? true
    :prev-rotation-week 46
    :leaves [[3 :soft]
             [4 :hard]
             [5 :hard]]}
   {:name "Ramya"
    :in-next-rotation? true
    :prev-rotation-week 47
    :leaves [[1 :soft]]}
   {:name "Samuel"
    :in-next-rotation? true
    :prev-rotation-week 48
    :leaves []}
   {:name "Rubal"
    :in-next-rotation? true
    :prev-rotation-week 49
    :leaves [[1 :hard]
             [2 :hard]]}
   {:name "Daniel"
    :in-next-rotation? true
    :prev-rotation-week 50
    :leaves [[14 :hard]
             [15 :hard]
             [16 :hard]
             [17 :hard]]}
   {:name "Mangal"
    :in-next-rotation? true
    :prev-rotation-week 51
    :leaves []}
   {:name "Setia"
    :in-next-rotation? true
    :prev-rotation-week 52
    :leaves []}
   {:name "Ketan"
    :in-next-rotation? true
    :prev-rotation-week 1
    :leaves []}
   {:name "Dinesh"
    :in-next-rotation? false
    :leaves []}
   {:name "Narendra"
    :in-next-rotation? true
    :leaves []}
   {:name "Mihil"
    :in-next-rotation? true
    :leaves []}
   {:name "Pranav"
    :in-next-rotation? true
    :leaves []}])

(def unique-test-rotation
  [{:name "Amogh"
    :in-next-rotation? true
    :prev-rotation-week 34
    :leaves []}
   {:name "Shantanu"
    :in-next-rotation? true
    :prev-rotation-week 38
    :leaves []}
   {:name "Suvrat"
    :in-next-rotation? true
    :prev-rotation-week 39
    :leaves []}
   {:name "Somya"
    :in-next-rotation? true
    :prev-rotation-week 40
    :leaves []}
   {:name "Shalaka"
    :in-next-rotation? true
    :prev-rotation-week 41
    :leaves []}
   {:name "Mourjo"
    :in-next-rotation? true
    :prev-rotation-week 42
    :leaves []}
   {:name "Neha"
    :in-next-rotation? true
    :prev-rotation-week 44
    :leaves []}
   {:name "Faiz"
    :in-next-rotation? true
    :prev-rotation-week 45
    :leaves []}
   {:name "Harsh"
    :in-next-rotation? true
    :prev-rotation-week 46
    :leaves [[3 :soft]
             [4 :hard]
             [5 :hard]]}
   {:name "Ramya"
    :in-next-rotation? true
    :prev-rotation-week 47
    :leaves [[1 :soft]]}
   {:name "Samuel"
    :in-next-rotation? true
    :prev-rotation-week 48
    :leaves []}
   {:name "Rubal"
    :in-next-rotation? true
    :prev-rotation-week 49
    :leaves [[1 :hard]
             [2 :hard]]}
   {:name "Daniel"
    :in-next-rotation? true
    :prev-rotation-week 50
    :leaves [[14 :hard]
             [15 :hard]
             [16 :hard]
             [17 :hard]]}
   {:name "Mangal"
    :in-next-rotation? true
    :prev-rotation-week 51
    :leaves []}
   {:name "Setia"
    :in-next-rotation? true
    :prev-rotation-week 52
    :leaves []}
   {:name "Ketan"
    :in-next-rotation? true
    :prev-rotation-week 1
    :leaves []}
   {:name "Narendra"
    :in-next-rotation? true
    :leaves []}
   {:name "Mihil"
    :in-next-rotation? true
    :leaves []}
   {:name "Pranav"
    :in-next-rotation? true
    :leaves []}])

(def test-base-plan
  {"Pranav"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}},
   "Harsh"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 46,
    :soft-leaves #{3},
    :hard-leaves #{4 5}},
   "Ramya"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 47,
    :soft-leaves #{1}},
   "Setia"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 52},
   "Ketan"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 1},
   "Shalaka"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 41},
   "Daniel"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 50,
    :hard-leaves #{14 15 16 17}},
   "Somya"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 40},
   "Shantanu"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 38},
   "Amogh"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 34},
   "Samuel"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 48},
   "Mihil"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}},
   "Mourjo"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 42},
   "Mangal"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 51},
   "Narendra"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}},
   "Neha"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 44},
   "Suvrat"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 39},
   "Rubal"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 49,
    :hard-leaves #{1 2}},
   "Faiz"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 45}})

(t/deftest test-uniquify-rotation-entries
  (t/is (= unique-test-rotation
           (sut/uniquify-rotation-entries test-rotation))))

(t/deftest test-add-person-to-plan
  (t/is (= {"Amogh" {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
                     :farthest-from 34}}
           (sut/add-person-to-plan #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
                                   {}
                                   (first unique-test-rotation))))
  (t/is (= {"Harsh" {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
                     :farthest-from 46,
                     :soft-leaves #{3},
                     :hard-leaves #{4 5}}}
           (sut/add-person-to-plan #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
                                   {}
                                   (first (drop 8 unique-test-rotation))))))

(t/deftest test-fill-base-values
  (t/is (= (sut/fill-base-values unique-test-rotation)
           [test-base-plan
            #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
            #{}])))

(t/deftest test-hard-leave-constraint?
  (t/is (sut/hard-leave-constraint? test-base-plan "Harsh" 4))
  (t/is (not (sut/hard-leave-constraint? test-base-plan "Amogh" 4))))

(t/deftest test-soft-leave-constraint?
  (t/is (sut/soft-leave-constraint? test-base-plan "Harsh" 3))
  (t/is (not (sut/soft-leave-constraint? (assoc test-base-plan
                                                "Harsh"
                                                {:next #{3},
                                                 :farthest-from 46,
                                                 :soft-leaves #{3},
                                                 :hard-leaves #{4 5}})
                                         "Harsh"
                                         3)))
  (t/is (not (sut/soft-leave-constraint? test-base-plan "Amogh" 4))))

(t/deftest test-already-eliminated?
  (t/is (sut/already-eliminated? (assoc test-base-plan
                                        "Harsh"
                                        {:next #{7 8},
                                         :farthest-from 46,
                                         :soft-leaves #{3},
                                         :hard-leaves #{4 5}})
                                 "Harsh"
                                 3))
  (t/is (not (sut/already-eliminated? (assoc test-base-plan
                                             "Harsh"
                                             {:next #{3 7 8},
                                              :farthest-from 46,
                                              :soft-leaves #{3},
                                              :hard-leaves #{4 5}})
                                      "Harsh"
                                      3))))

(t/deftest test-already-assigned?
  (t/is (sut/already-assigned? (assoc test-base-plan
                                      "Harsh"
                                      {:next #{3},
                                       :farthest-from 46,
                                       :soft-leaves #{3},
                                       :hard-leaves #{4 5}})
                                 "Harsh"
                                 7))
  (t/is (not (sut/already-assigned? (assoc test-base-plan
                                           "Harsh"
                                           {:next #{3 7},
                                            :farthest-from 46,
                                            :soft-leaves #{3},
                                            :hard-leaves #{4 5}})
                                    "Harsh"
                                    7))))

(t/deftest test-eliminate-week
  (let [res (sut/eliminate-week test-base-plan "Harsh" 3)]
    (t/is (= {:next #{7 20 4 15 13 6 17 12 2 19 11 9 5 14 16 10 18 8},
              :farthest-from 46,
              :soft-leaves #{3},
              :hard-leaves #{4 5}}
             (get res "Harsh")))))

(t/deftest test-assign-week
  ;; soft constraint
  (t/is (nil? (sut/assign-week test-base-plan "Harsh" 3)))
  ;; hard constraint
  (t/is (nil? (sut/assign-week test-base-plan "Harsh" 4)))
  (t/is (nil? (sut/assign-week test-base-plan "Harsh" 5)))
  ;; no other value constraint
  (t/is (nil? (sut/assign-week (assoc test-base-plan
                                      "Faiz"
                                      {:next #{6},
                                       :farthest-from 45})
                               "Harsh"
                               6)))
  ;; (t/is (= (sut/assign-week test-base-plan "Harsh" 6)
  ;;          (apply hash-map
  ;;                 (mapcat (fn [[k v]]
  ;;                           (if (= k "Harsh")
  ;;                             [k (assoc v :next #{6})]
  ;;                             [k (update v :next disj 6)]))
  ;;                         test-base-plan))))
  )

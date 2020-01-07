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
    :constraints []}
   {:name "Pavitra"
    :in-next-rotation? false
    :prev-rotation-week 33
    :constraints []}
   {:name "Amogh"
    :in-next-rotation? true
    :prev-rotation-week 34
    :constraints []}
   {:name "Daniel"
    :in-next-rotation? true
    :prev-rotation-week 35
    :assist "Shantanu"
    :constraints []}
   {:name "Mangal"
    :in-next-rotation? true
    :prev-rotation-week 36
    :assist "Shalaka"
    :constraints []}
   {:name "Setia"
    :in-next-rotation? true
    :prev-rotation-week 37
    :assist "Mourjo"
    :constraints []}
   {:name "Shantanu"
    :in-next-rotation? true
    :prev-rotation-week 38
    :constraints []}
   {:name "Suvrat"
    :in-next-rotation? true
    :prev-rotation-week 39
    :constraints []}
   {:name "Somya"
    :in-next-rotation? true
    :prev-rotation-week 40
    :constraints []}
   {:name "Shalaka"
    :in-next-rotation? true
    :prev-rotation-week 41
    :constraints []}
   {:name "Mourjo"
    :in-next-rotation? true
    :prev-rotation-week 42
    :constraints []}
   {:name "Vedang"
    :in-next-rotation? false
    :prev-rotation-week 43
    :constraints [[1 :soft :leave]]}
   {:name "Neha"
    :in-next-rotation? true
    :prev-rotation-week 44
    :constraints []}
   {:name "Faiz"
    :in-next-rotation? true
    :prev-rotation-week 45
    :constraints []}
   {:name "Harsh"
    :in-next-rotation? true
    :prev-rotation-week 46
    :constraints [[3 :soft :leave]
                  [4 :hard :leave]
                  [5 :hard :leave]]}
   {:name "Ramya"
    :in-next-rotation? true
    :prev-rotation-week 47
    :constraints [[1 :soft :leave]]}
   {:name "Samuel"
    :in-next-rotation? true
    :prev-rotation-week 48
    :constraints []}
   {:name "Rubal"
    :in-next-rotation? true
    :prev-rotation-week 49
    :constraints [[1 :hard :leave]
                  [2 :hard :leave]]}
   {:name "Daniel"
    :in-next-rotation? true
    :prev-rotation-week 50
    :constraints [[14 :hard :leave]
                  [15 :hard :leave]
                  [16 :hard :leave]
                  [17 :hard :leave]]}
   {:name "Mangal"
    :in-next-rotation? true
    :prev-rotation-week 51
    :constraints []}
   {:name "Setia"
    :in-next-rotation? true
    :prev-rotation-week 52
    :constraints []}
   {:name "Ketan"
    :in-next-rotation? true
    :prev-rotation-week 1
    :constraints []}
   {:name "Dinesh"
    :in-next-rotation? false
    :constraints []}
   {:name "Narendra"
    :in-next-rotation? true
    :constraints []}
   {:name "Mihil"
    :in-next-rotation? true
    :constraints []}
   {:name "Pranav"
    :in-next-rotation? true
    :constraints []}])

(def unique-test-rotation
  [{:name "Amogh"
    :in-next-rotation? true
    :prev-rotation-week 34
    :constraints []}
   {:name "Shantanu"
    :in-next-rotation? true
    :prev-rotation-week 38
    :constraints []}
   {:name "Suvrat"
    :in-next-rotation? true
    :prev-rotation-week 39
    :constraints []}
   {:name "Somya"
    :in-next-rotation? true
    :prev-rotation-week 40
    :constraints []}
   {:name "Shalaka"
    :in-next-rotation? true
    :prev-rotation-week 41
    :constraints []}
   {:name "Mourjo"
    :in-next-rotation? true
    :prev-rotation-week 42
    :constraints []}
   {:name "Neha"
    :in-next-rotation? true
    :prev-rotation-week 44
    :constraints []}
   {:name "Faiz"
    :in-next-rotation? true
    :prev-rotation-week 45
    :constraints []}
   {:name "Harsh"
    :in-next-rotation? true
    :prev-rotation-week 46
    :constraints [[3 :soft :leave]
                  [4 :hard :leave]
                  [5 :hard :leave]]}
   {:name "Ramya"
    :in-next-rotation? true
    :prev-rotation-week 47
    :constraints [[1 :soft :leave]]}
   {:name "Samuel"
    :in-next-rotation? true
    :prev-rotation-week 48
    :constraints []}
   {:name "Rubal"
    :in-next-rotation? true
    :prev-rotation-week 49
    :constraints [[1 :hard :leave]
                  [2 :hard :leave]]}
   {:name "Daniel"
    :in-next-rotation? true
    :prev-rotation-week 50
    :constraints [[14 :hard :leave]
                  [15 :hard :leave]
                  [16 :hard :leave]
                  [17 :hard :leave]]}
   {:name "Mangal"
    :in-next-rotation? true
    :prev-rotation-week 51
    :constraints []}
   {:name "Setia"
    :in-next-rotation? true
    :prev-rotation-week 52
    :constraints []}
   {:name "Ketan"
    :in-next-rotation? true
    :prev-rotation-week 1
    :constraints []}
   {:name "Narendra"
    :in-next-rotation? true
    :constraints []}
   {:name "Mihil"
    :in-next-rotation? true
    :constraints []}
   {:name "Pranav"
    :in-next-rotation? true
    :constraints []}])

(def test-base-plan
  {"Pranav"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}},
   "Harsh"
   {:next #{7 20 15 13 6 17 3 12 2 19 11 9 14 16 10 18 8},
    :farthest-from 46,
    :soft-constraints #{3},
    :hard-constraints #{4 5}},
   "Ramya"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :farthest-from 47,
    :soft-constraints #{1}},
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
   {:next #{7 20 4 13 6 3 12 2 19 11 9 5 10 18 8},
    :farthest-from 50,
    :hard-constraints #{14 15 16 17}},
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
   {:next #{7 20 4 15 13 6 17 3 12 19 11 9 5 14 16 10 18 8},
    :farthest-from 49,
    :hard-constraints #{1 2}},
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
  (t/is (= {"Harsh" {:next #{7 20 15 13 6 17 3 12 2 19 11 9 14 16 10 18 8},
                     :farthest-from 46,
                     :soft-constraints #{3},
                     :hard-constraints #{4 5}}}
           (sut/add-person-to-plan #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
                                   {}
                                   (first (drop 8 unique-test-rotation))))))

(t/deftest test-fill-base-values
  (let [res (sut/fill-base-values unique-test-rotation)]
    (t/is (= res
             [test-base-plan
              '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
              #{}]))
    (t/is (= (count unique-test-rotation)
             (count (second res))))))

(t/deftest test-hard-leave-constraint?
  (t/is (sut/hard-leave-constraint? test-base-plan "Harsh" 4))
  (t/is (not (sut/hard-leave-constraint? test-base-plan "Amogh" 4))))

(t/deftest test-soft-leave-constraint?
  (t/is (sut/soft-leave-constraint? test-base-plan "Harsh" 3))
  (t/is (not (sut/soft-leave-constraint? (assoc test-base-plan
                                                "Harsh"
                                                {:next #{3},
                                                 :farthest-from 46,
                                                 :soft-constraints #{3},
                                                 :hard-constraints #{4 5}})
                                         "Harsh"
                                         3)))
  (t/is (not (sut/soft-leave-constraint? test-base-plan "Amogh" 4))))

(t/deftest test-already-eliminated?
  (t/is (sut/already-eliminated? (assoc test-base-plan
                                        "Harsh"
                                        {:next #{7 8},
                                         :farthest-from 46,
                                         :soft-constraints #{3},
                                         :hard-constraints #{4 5}})
                                 "Harsh"
                                 3))
  (t/is (not (sut/already-eliminated? (assoc test-base-plan
                                             "Harsh"
                                             {:next #{3 7 8},
                                              :farthest-from 46,
                                              :soft-constraints #{3},
                                              :hard-constraints #{4 5}})
                                      "Harsh"
                                      3))))

(t/deftest test-already-assigned?
  (t/is (sut/already-assigned? (assoc test-base-plan
                                      "Harsh"
                                      {:next #{3},
                                       :farthest-from 46,
                                       :soft-constraints #{3},
                                       :hard-constraints #{4 5}})
                               "Harsh"))
  (t/is (not (sut/already-assigned? (assoc test-base-plan
                                           "Harsh"
                                           {:next #{3 7},
                                            :farthest-from 46,
                                            :soft-constraints #{3},
                                            :hard-constraints #{4 5}})
                                    "Harsh"))))

(t/deftest test-eliminate-week
  (t/is (= {:next #{7 20 15 13 6 17 12 2 19 11 9 14 16 10 18 8},
            :farthest-from 46,
            :soft-constraints #{3},
            :hard-constraints #{4 5}}
           (get (sut/eliminate-week test-base-plan "Harsh" 3)
                "Harsh")))
  (t/is (nil? (sut/eliminate-week (assoc test-base-plan
                                         "Harsh"
                                         {:next #{3}
                                          :farthest-from 46
                                          :soft-constraints #{3}
                                          :hard-constraints #{4 5}})
                                  "Harsh"
                                  3)))
  (t/is (nil? (sut/eliminate-week (assoc test-base-plan
                                         "Harsh"
                                         {:next #{3 4}
                                          :farthest-from 46
                                          :soft-constraints #{3}
                                          :hard-constraints #{4 5}})
                                  "Harsh"
                                  3)))
  (let [res (sut/eliminate-week (assoc test-base-plan
                                       "Harsh"
                                       {:next #{3 6 7}
                                        :farthest-from 46
                                        :soft-constraints #{3}
                                        :hard-constraints #{4 5}})
                                "Harsh"
                                3)]
    (t/is (= #{6 7} (get-in res ["Harsh" :next]))))
  (let [res (sut/eliminate-week (assoc test-base-plan
                                       "Harsh"
                                       {:next #{3 6}
                                        :farthest-from 46
                                        :soft-constraints #{3}
                                        :hard-constraints #{4 5}})
                                "Harsh"
                                3)]
    (t/is (= #{6} (get-in res ["Harsh" :next])))))

(t/deftest test-assign-week+eliminate-week-for-others
  (let [res (sut/assign-week+eliminate-week-for-others
             (assoc test-base-plan
                    "Harsh"
                    {:next #{3 6}
                     :farthest-from 46
                     :soft-constraints #{3}
                     :hard-constraints #{4 5}}
                    "Faiz"
                    {:next #{3 6}
                     :farthest-from 45})
             "Harsh"
             6)]
    (t/is (= #{6} (get-in res ["Harsh" :next])))
    (t/is (= #{3} (get-in res ["Faiz" :next])))
    (t/is (nil? (some #{3 6} (get-in res ["Mourjo" :next]))))))

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
  (t/is (= (sut/assign-week test-base-plan "Harsh" 6)
           (apply hash-map
                  (mapcat (fn [[k v]]
                            (if (= k "Harsh")
                              [k (assoc v :next #{6})]
                              [k (update v :next disj 6)]))
                          test-base-plan)))))

(t/deftest test-swapper
  (t/is (= (sut/swapper 1 [1 2 3 4 5 6])
           [2 1 3 4 5 6]))
  (t/is (= (sut/swapper 2 [1 2 3 4 5 6])
           [3 1 2 4 5 6]))
  (t/is (= (sut/swapper 3 [1 2 3 4 5 6])
           [4 1 2 3 5 6])))

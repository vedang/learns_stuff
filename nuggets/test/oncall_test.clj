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

(def unique-test-rotation-2
  [{:name "Harsh"
    :in-next-rotation? true
    :prev-rotation-week 49
    :constraints [[52 :hard :leave]
                  [1 :hard :leave]
                  [2 :hard :leave]]}
   {:name "Ramya"
    :in-next-rotation? true
    :prev-rotation-week 50
    :constraints [[1 :soft :leave]
                  [3 :soft :leave]
                  [4 :hard :leave]]}
   {:name "Samuel"
    :in-next-rotation? true
    :prev-rotation-week 51
    :constraints [[1 :soft :leave]
                  [2 :hard :leave]]}
   {:name "Pranav"
    :in-next-rotation? true
    :constraints [[1 :hard :leave]
                  [2 :hard :leave]]}])

(def test-base-plan
  {"Pranav"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? false},
   "Harsh"
   {:next #{7 20 15 13 6 17 3 12 2 19 11 9 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 46,
    :ideal-week 13,
    :soft-constraints #{3},
    :hard-constraints #{4 5}},
   "Ramya"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 47,
    :ideal-week 14,
    :soft-constraints #{1}},
   "Setia"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 52,
    :ideal-week 19},
   "Ketan"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? false,
    :prev 1,
    :ideal-week 20},
   "Shalaka"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 41,
    :ideal-week 8},
   "Daniel"
   {:next #{7 20 4 13 6 3 12 2 19 11 9 5 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 50,
    :ideal-week 17,
    :hard-constraints #{14 15 16 17}},
   "Somya"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 40,
    :ideal-week 7},
   "Shantanu"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 38,
    :ideal-week 5},
   "Amogh"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 34,
    :ideal-week 1},
   "Samuel"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 48,
    :ideal-week 15},
   "Mihil"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? false},
   "Mourjo"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 42,
    :ideal-week 9},
   "Mangal"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 51,
    :ideal-week 18},
   "Narendra"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? false},
   "Neha"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 44,
    :ideal-week 11},
   "Suvrat"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 39,
    :ideal-week 6},
   "Rubal"
   {:next #{7 20 4 15 13 6 17 3 12 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 49,
    :ideal-week 16,
    :hard-constraints #{1 2}},
   "Faiz"
   {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8},
    :ideal-distance 19,
    :week-wrap-around? true,
    :prev 45,
    :ideal-week 12}})

(def test-base-plan-2
  {"Harsh"
   {:next #{3},
    :ideal-distance 4,
    :week-wrap-around? true,
    :prev 49,
    :ideal-week 1,
    :hard-constraints #{1 2 52}},
   "Ramya"
   {:next #{52 1 2 3},
    :ideal-distance 4,
    :week-wrap-around? true,
    :prev 50,
    :ideal-week 2,
    :soft-constraints #{1 3},
    :hard-constraints #{4}},
   "Samuel"
   {:next #{52 1 3},
    :ideal-distance 4,
    :week-wrap-around? true,
    :prev 51,
    :ideal-week 3,
    :soft-constraints #{1},
    :hard-constraints #{2}},
   "Pranav"
   {:next #{52 3},
    :ideal-distance 4,
    :week-wrap-around? false,
    :hard-constraints #{1 2}}})

(t/deftest test-uniquify-rotation-entries
  (t/is (= unique-test-rotation
           (sut/uniquify-rotation-entries test-rotation))))

(t/deftest test-add-person-to-plan
  (t/is (= {"Amogh" {:next #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
                     :ideal-distance 19
                     :week-wrap-around? true
                     :ideal-week 1
                     :prev 34}}
           (sut/add-person-to-plan #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
                                   {}
                                   (first unique-test-rotation))))
  (t/is (= {"Harsh" {:next #{7 20 15 13 6 17 3 12 2 19 11 9 14 16 10 18 8},
                     :prev 46,
                     :ideal-distance 19
                     :week-wrap-around? true
                     :ideal-week 13
                     :soft-constraints #{3},
                     :hard-constraints #{4 5}}}
           (sut/add-person-to-plan #{7 20 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}
                                   {}
                                   (first (drop 8 unique-test-rotation))))))

(t/deftest test-fill-base-values
  (let [res (sut/fill-base-values unique-test-rotation)]
    (t/is (= res
             [test-base-plan
              '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)]))
    (t/is (= (count unique-test-rotation)
             (count (second res)))))

  (let [res (sut/fill-base-values unique-test-rotation-2)]
    (t/is (= res
             [test-base-plan-2
              '(52 1 2 3)]))
    (t/is (= (count unique-test-rotation-2)
             (count (second res))))))

(t/deftest test-optimize-base-values
  (t/is (= [test-base-plan
            (map :name unique-test-rotation)
            '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)]
           (sut/optimize-base-values test-base-plan
                                     (map :name unique-test-rotation)
                                     '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))))
  (t/is (= [{"Harsh"
             {:next #{3},
              :ideal-distance 4,
              :week-wrap-around? true,
              :prev 49,
              :ideal-week 1,
              :hard-constraints #{1 2 52}},
             "Ramya"
             {:next #{2},
              :ideal-distance 4,
              :week-wrap-around? true,
              :prev 50,
              :ideal-week 2,
              :soft-constraints #{1 3},
              :hard-constraints #{4}},
             "Samuel"
             {:next #{1},
              :ideal-distance 4,
              :week-wrap-around? true,
              :prev 51,
              :ideal-week 3,
              :soft-constraints #{1},
              :hard-constraints #{2}},
             "Pranav"
             {:next #{52},
              :ideal-distance 4,
              :week-wrap-around? false,
              :hard-constraints #{1 2}}}
            '()
            '()]
           (sut/optimize-base-values test-base-plan-2
                                     (map :name unique-test-rotation-2)
                                     '(52 1 2 3)))
        "Optimizing the base plan solves the whole plan itself!")
  ;; conflicting assignment, both Harsh and Pranav need 3
  (t/is (nil? (sut/optimize-base-values
               (-> test-base-plan-2
                   (update-in ["Pranav" :hard-constraints] conj 52)
                   (update-in ["Pranav" :next] disj 52))
               (map :name unique-test-rotation-2)
               '(52 1 2 3))))
  ;; no possible week for Harsh
  (t/is (nil? (sut/optimize-base-values
               (-> test-base-plan-2
                   (update-in ["Harsh" :hard-constraints] conj 3)
                   (update-in ["Harsh" :next] disj 3))
               (map :name unique-test-rotation-2)
               '(52 1 2 3)))))

(t/deftest test-soft-leave-constraint?
  (t/is (sut/soft-leave-constraint? test-base-plan "Harsh" 3))
  (t/is (not (sut/soft-leave-constraint? (assoc test-base-plan
                                                "Harsh"
                                                {:next #{3},
                                                 :prev 46,
                                                 :soft-constraints #{3},
                                                 :hard-constraints #{4 5}})
                                         "Harsh"
                                         3)))
  (t/is (not (sut/soft-leave-constraint? test-base-plan "Amogh" 4))))

(t/deftest test-already-eliminated?
  (t/is (sut/already-eliminated? (assoc test-base-plan
                                        "Harsh"
                                        {:next #{7 8},
                                         :prev 46,
                                         :soft-constraints #{3},
                                         :hard-constraints #{4 5}})
                                 "Harsh"
                                 3))
  (t/is (not (sut/already-eliminated? (assoc test-base-plan
                                             "Harsh"
                                             {:next #{3 7 8},
                                              :prev 46,
                                              :soft-constraints #{3},
                                              :hard-constraints #{4 5}})
                                      "Harsh"
                                      3))))

(t/deftest test-already-assigned?
  (t/is (sut/already-assigned? (assoc test-base-plan
                                      "Harsh"
                                      {:next #{3},
                                       :prev 46,
                                       :soft-constraints #{3},
                                       :hard-constraints #{4 5}})
                               "Harsh"))
  (t/is (not (sut/already-assigned? (assoc test-base-plan
                                           "Harsh"
                                           {:next #{3 7},
                                            :prev 46,
                                            :soft-constraints #{3},
                                            :hard-constraints #{4 5}})
                                    "Harsh"))))

(t/deftest test-get-assigned-week
  (t/is (= (sut/get-assigned-week (assoc test-base-plan
                                         "Harsh"
                                         {:next #{3},
                                          :prev 46,
                                          :soft-constraints #{3},
                                          :hard-constraints #{4 5}})
                                  "Harsh")
           3)
        "Returns the assigned week for a person")
  (t/is (nil? (sut/get-assigned-week (assoc test-base-plan
                                            "Harsh"
                                            {:next #{3 7},
                                             :prev 46,
                                             :soft-constraints #{3},
                                             :hard-constraints #{4 5}})
                                     "Harsh"))
        "Returns nil when a single week has not been assigned to a person."))

(t/deftest test-eliminate-week
  (t/is (= {:next #{7 20 15 13 3 17 12 2 19 11 9 14 16 10 18 8},
            :prev 46,
            :ideal-distance 19,
            :week-wrap-around? true,
            :ideal-week 13
            :soft-constraints #{3},
            :hard-constraints #{4 5}}
           (get (sut/eliminate-week test-base-plan "Harsh" 6)
                "Harsh"))
        "Basic Elimination should work")
  (t/is (nil? (sut/eliminate-week (assoc test-base-plan
                                         "Harsh"
                                         {:next #{3}
                                          :prev 46
                                          :soft-constraints #{3}
                                          :hard-constraints #{4 5}})
                                  "Harsh"
                                  3))
        "We cannot eliminate the last possible value, as the resulting plan is invalid.")
  (let [res (sut/eliminate-week (assoc test-base-plan
                                       "Harsh"
                                       {:next #{3 6 7}
                                        :prev 46
                                        :soft-constraints #{3}
                                        :hard-constraints #{4 5}})
                                "Harsh"
                                3)]
    (t/is (= #{6 7} (get-in res ["Harsh" :next]))
          "Eliminating a soft constraint is allowed"))
  (let [res (sut/eliminate-week (assoc test-base-plan
                                       "Harsh"
                                       {:next #{3 6}
                                        :prev 46
                                        :soft-constraints #{3}
                                        :hard-constraints #{4 5}})
                                "Harsh"
                                6)]
    (t/is (= #{3} (get-in res ["Harsh" :next]))
          "If the final week left after elimination is a soft constraint, this is allowed.")))

(t/deftest test-eliminate-week-for-others
  (let [res (sut/eliminate-week-for-others test-base-plan "Harsh" 6)]
    (t/is (every? (comp nil? (partial some #{6}) :next val)
                  (dissoc res "Harsh"))
          "The week is eliminated as a possibility for every other person."))
  (let [res (sut/eliminate-week-for-others
             (assoc test-base-plan
                    "Harsh"
                    {:next #{3 6}
                     :prev 46
                     :soft-constraints #{3}
                     :hard-constraints #{4 5}})
             "Faiz"
             6)]
    (t/is (every? (comp nil? (partial some #{6 3}) :next val)
                  (dissoc res "Harsh" "Faiz"))
          "If eliminating a week for someone leaves that person with a single choice, that choice is also propagated to others.")))

(t/deftest test-assign-week
  (t/is (nil? (sut/assign-week test-base-plan "Harsh" 3))
        "Soft constraints are rejected when another option is available.")

  (t/is (nil? (sut/assign-week test-base-plan "Harsh" 4))
        "Hard constraints are not part of the next set and are rejected")
  (t/is (nil? (sut/assign-week test-base-plan "Harsh" 5))
        "Hard constraints are not part of the next set and are rejected")

  (t/is (nil? (sut/assign-week (assoc test-base-plan
                                      "Faiz"
                                      {:next #{6},
                                       :prev 45})
                               "Harsh"
                               6))
        "If an assignment attempt for person A leaves person B with no other choice, it is rejected.")

  (t/is (= (sut/assign-week test-base-plan "Harsh" 6)
           (apply hash-map
                  (mapcat (fn [[k v]]
                            (if (= k "Harsh")
                              [k (assoc v :next #{6})]
                              [k (update v :next disj 6)]))
                          test-base-plan)))
        "Assigning a week to one person means eliminating it for others.")

  (t/is (= (sut/assign-week (assoc-in test-base-plan ["Faiz" :next] #{6 4})
                            "Harsh"
                            6)
           (apply hash-map
                  (mapcat (fn [[k v]]
                            (cond
                              (= k "Harsh")
                              [k (assoc v :next #{6})]
                              (= k "Faiz")
                              [k (assoc v :next #{4})]
                              :else
                              [k (update v :next disj 6 4)]))
                          test-base-plan)))
        "If assigning a week to person A only leaves 1 option for person B, this is also assigned to B and propagated to others."))

(t/deftest test-generate-plan
  (t/is (= test-base-plan-2
           (sut/generate-plan test-base-plan-2 '() '(52 1 2 3)))
        "Empty names array means the plan is generated.")
  (t/is (= test-base-plan-2
           (sut/generate-plan test-base-plan-2 (map :name test-base-plan-2) '()))
        "Empty weeks array means the plan is generated.")
  (let [plan {"Samuel"
              {:next #{1 52},
               :ideal-distance 2,
               :week-wrap-around? true,
               :prev 51,
               :ideal-week 1,
               :soft-constraints #{1 52}},
              "Pranav"
              {:next #{1 52},
               :ideal-distance 2,
               :week-wrap-around? false,
               :soft-constraints #{1 52}}}
        names '("Samuel" "Pranav")
        weeks '(52 1)]
    (t/is (= (sut/generate-plan plan names weeks)
             {"Samuel"
              {:next #{1 52},
               :ideal-distance 2,
               :week-wrap-around? true,
               :prev 51,
               :ideal-week 1,
               :soft-constraints #{1 52}},
              "Pranav"
              {:next #{1 52},
               :ideal-distance 2,
               :week-wrap-around? false,
               :soft-constraints #{1 52}}})
          "Constraint Propagation won't be able to assign a week here.")))

;;; Other tests for `sut/generate-plan` are covered under tests for
;;; `sut/next-rotation`

(t/deftest test-next-rotation
  (let [base-rotation [{:name "Harsh"
                        :in-next-rotation? true
                        :prev-rotation-week 50
                        :constraints [[3 :soft :leave]
                                      [4 :hard :leave]
                                      [5 :hard :leave]]}
                       {:name "Ramya"
                        :in-next-rotation? true
                        :prev-rotation-week 51
                        :constraints [[1 :soft :leave]
                                      [5 :hard :leave]
                                      [6 :hard :leave]]}
                       {:name "Samuel"
                        :in-next-rotation? true
                        :prev-rotation-week 52
                        :constraints []}
                       {:name "Pranav"
                        :in-next-rotation? true
                        :constraints [[1 :hard :leave]
                                      [2 :hard :leave]]}]]
    (t/is (= (sut/next-rotation base-rotation)
             {"Harsh"
              {:next #{1},
               :ideal-distance 4,
               :week-wrap-around? true,
               :prev 50,
               :ideal-week 2,
               :soft-constraints #{3},
               :hard-constraints #{4 5}},
              "Ramya"
              {:next #{2},
               :ideal-distance 4,
               :week-wrap-around? true,
               :prev 51,
               :ideal-week 3,
               :soft-constraints #{1},
               :hard-constraints #{6 5}},
              "Samuel"
              {:next #{3},
               :ideal-distance 4,
               :week-wrap-around? true,
               :prev 52,
               :ideal-week 4},
              "Pranav"
              {:next #{4},
               :ideal-distance 4,
               :week-wrap-around? false,
               :hard-constraints #{1 2}}})))

  (let [base-rotation [{:name "Harsh"
                        :in-next-rotation? true
                        :prev-rotation-week 49
                        :constraints [[52 :hard :leave]
                                      [1 :hard :leave]
                                      [2 :hard :leave]]}
                       {:name "Ramya"
                        :in-next-rotation? true
                        :prev-rotation-week 50
                        :constraints [[1 :soft :leave]
                                      [3 :soft :leave]
                                      [4 :hard :leave]]}
                       {:name "Samuel"
                        :in-next-rotation? true
                        :prev-rotation-week 51
                        :constraints [[1 :soft :leave]
                                      [2 :hard :leave]]}
                       {:name "Pranav"
                        :in-next-rotation? true
                        :constraints [[1 :hard :leave]
                                      [2 :hard :leave]]}]]
    (t/is (= (sut/next-rotation base-rotation)
             {"Harsh"
              {:next #{3},
               :ideal-distance 4,
               :week-wrap-around? true,
               :prev 49,
               :ideal-week 1,
               :hard-constraints #{1 2 52}},
              "Ramya"
              {:next #{2},
               :ideal-distance 4,
               :week-wrap-around? true,
               :prev 50,
               :ideal-week 2,
               :soft-constraints #{1 3},
               :hard-constraints #{4}},
              "Samuel"
              {:next #{1},
               :ideal-distance 4,
               :week-wrap-around? true,
               :prev 51,
               :ideal-week 3,
               :soft-constraints #{1},
               :hard-constraints #{2}},
              "Pranav"
              {:next #{52},
               :ideal-distance 4,
               :week-wrap-around? false,
               :hard-constraints #{1 2}}})
          "Optimize base values itself solves this plan."))

  (let [base-rotation [{:name "Samuel"
                        :in-next-rotation? true
                        :prev-rotation-week 51
                        :constraints [[1 :soft :leave]]}
                       {:name "Pranav"
                        :in-next-rotation? true
                        :constraints [[52 :soft :leave]]}]]
    (t/is (= (sut/next-rotation base-rotation)
             {"Samuel"
              {:next #{52},
               :ideal-distance 2,
               :week-wrap-around? true,
               :prev 51,
               :ideal-week 1,
               :soft-constraints #{1}},
              "Pranav"
              {:next #{1},
               :ideal-distance 2,
               :week-wrap-around? false,
               :soft-constraints #{52}}})))

  (let [base-rotation [{:name "Samuel"
                        :in-next-rotation? true
                        :prev-rotation-week 51
                        :constraints [[52 :soft :leave]]}
                       {:name "Pranav"
                        :in-next-rotation? true
                        :constraints [[52 :soft :leave]]}]]
    (t/is (= (sut/next-rotation base-rotation)
             {"Samuel"
              {:next #{1},
               :ideal-distance 2,
               :week-wrap-around? true,
               :prev 51,
               :ideal-week 1,
               :soft-constraints #{52}},
              "Pranav"
              {:next #{52},
               :ideal-distance 2,
               :week-wrap-around? false,
               :soft-constraints #{52}}})))

  (let [base-rotation [{:name "Samuel"
                        :in-next-rotation? true
                        :prev-rotation-week 51
                        :constraints [[52 :soft :leave]
                                      [1 :soft :leave]]}
                       {:name "Pranav"
                        :in-next-rotation? true
                        :constraints [[52 :soft :leave]
                                      [1 :soft :leave]]}]]
    (t/is (= (sut/next-rotation base-rotation)
             {"Samuel"
              {:next #{1},
               :ideal-distance 2,
               :week-wrap-around? true,
               :prev 51,
               :ideal-week 1,
               :soft-constraints #{1 52}},
              "Pranav"
              {:next #{52},
               :ideal-distance 2,
               :week-wrap-around? false,
               :soft-constraints #{1 52}}})
          "Constraint Propagation won't be able to assign a week here, but search does.")))

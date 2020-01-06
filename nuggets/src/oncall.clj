(ns oncall
  "Given one oncall rotation schedule, generate the next one.")

(def prev-rotation
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

(defn uniquify-rotation-entries
  "Keep only the latest rotation information for any person, also keep
  only those people who will participate in the next rotation."
  [rotation]
  (let [[_ unique-rotation]
        (reduce (fn [[seen-name? new-rotation]
                    {person-name :name
                     keep-person? :in-next-rotation?
                     :as p}]
                  (cond
                    (seen-name? person-name)
                    [seen-name? new-rotation]

                    ;; This person is not participating in the next rotation
                    ;; We've seen them, but we do nothing
                    (not keep-person?)
                    [(conj seen-name? person-name)
                     new-rotation]

                    :else
                    [(conj seen-name? person-name)
                     (conj new-rotation p)]))
                ;; Works by reading the rotation in reverse order, and
                ;; keeping track of unique names we see. Using a list
                ;; for the internal rotation allows conj to build the
                ;; final list in the correct order (by appending new
                ;; entries to the head of the list instead of the
                ;; tail.
                [#{} '()]
                (reverse rotation))]
    ;; Change the list back to vector to respect external return
    ;; types.
    (vec unique-rotation)))

(defn add-person-to-plan
  "Given a plan and a person object, add the relevant details of the
  person to the plan."
  [next-values plan {person-name :name
                     prev-rotation :prev-rotation-week
                     leaves :leaves}]
  (let [[soft-leaves hard-leaves]
        (reduce (fn [[sl hl] [lweek ltype]]
                  (if (= ltype :soft)
                    [(conj sl lweek) hl]
                    [sl (conj hl lweek)]))
                [#{} #{}]
                leaves)]
    (assoc plan
           person-name (merge {:next next-values}
                              (when prev-rotation
                                {:farthest-from prev-rotation})
                              (when (seq soft-leaves)
                                {:soft-leaves soft-leaves})
                              (when (seq hard-leaves)
                                {:hard-leaves hard-leaves})))))

(defn fill-base-values
  "Given a unique rotation, returns a `base-plan`, a set of
  `weeks-to-assign` and a set of `assigned-weeks` (empty, since
  nothing is assigned at the moment.)"
  [unique-rotation]
  (let [num-weeks (count unique-rotation)
        ;; Which week does the next rotation start from?
        week-counter (->> unique-rotation
                          reverse
                          (keep :prev-rotation-week)
                          first
                          inc)
        weeks-to-assign (set (range week-counter
                                    (+ week-counter num-weeks)))
        base-plan (reduce (partial add-person-to-plan weeks-to-assign)
                          {}
                          unique-rotation)]

    [base-plan weeks-to-assign #{}]))

(defn hard-leave-constraint?
  "Is this allocation hitting a hard constraint? If so, return true."
  [curr-plan person-name week]
  (and (get-in curr-plan [person-name :hard-leaves])
       ((get-in curr-plan [person-name :hard-leaves]) week)))

(defn soft-leave-constraint?
  "Is this allocation hitting a soft constraint? If so, return true."
  [curr-plan person-name week]
  (and (get-in curr-plan [person-name :soft-leaves])
       ((get-in curr-plan [person-name :soft-leaves]) week)
       ;; other assignment options are currently possible
       (> (count (get-in curr-plan [person-name :next])) 1)))

(defn already-eliminated?
  "Return true if this allocation is already eliminated for the person."
  [curr-plan person-name week]
  (not ((get-in curr-plan [person-name :next]) week)))

(defn already-assigned?
  "Return true if this person only has a single week in their next
  slot (meaning that allocation is already done for the person)."
  [curr-plan person-name week]
  (= 1 (count (get-in curr-plan [person-name :next]))))

(declare assign-week)

(defn eliminate-week
  "Assumes that the week can be eliminated for the given person. If
  this leads to the person having only a single week left, this
  information is further propagated in the plan."
  [plan person-name week]
  (let [person-val (get plan person-name)
        curr-possibilities (:next person-val)
        new-possibilities (disj curr-possibilities week)
        new-plan (assoc plan
                        person-name (assoc person-val
                                           :next new-possibilities))]
    (cond
      ;; Removing this week from the set of possibilities leaves the
      ;; person with no more options. This is a contradiction.
      (empty? new-possibilities) nil
      ;; Removing this week has left only one option for this person.
      ;; This should be propagated as the week for this person to
      ;; everyone else.
      (and (= (count new-possibilities) 1)
           (> (count curr-possibilities) 1))
      (assign-week new-plan person-name (first new-possibilities))
      ;; Elimination is complete, return the new plan.
      :else
      new-plan)))

(defn assign-week+eliminate-week-for-others
  "We're assigning the week to the specified person. Update the plan
  to eliminate the week for others.

  If this leaves anyone with only one other option, propagate that
  option into the plan as well."
  [curr-plan person-name week]
  (when-let [updated-plan
             (reduce (fn [new-plan [pname {possible-vals :next
                                          :as pval}]]
                       (if (= pname person-name)
                         ;; Do nothing in this reduction, we are
                         ;; trying to assign this week to this person
                         ;; and using this reduction to eliminate the
                         ;; week from everyone else.
                         new-plan
                         ;; Eliminate this week for everyone else.
                         (if-let [new-plan (eliminate-week new-plan person-name week)]
                           new-plan
                           (reduced nil))))
                     curr-plan
                     curr-plan)]
    ;; This assignment is possible. Make it and return the plan.
    (assoc updated-plan person-name #{week})))

(defn assign-week
  "Trying to assign a week to a person means the following:

  1. Check if this is a valid assignment (the next vector contains
  this week).

  2. Check if there is a hard constraint against this person for this
  week. In this case, assignment is not possible.

  3. Check if there is a soft constraint against this person for this
  week. In this case, if there are other options available, consider
  the assignment as not possible. There is an obvious bug in this
  logic, in that some other hard constraint might knock out the
  assignment we've made later. I'll think about this later.

  4. Remove that week as a possibility for everyone else. If it's the
  last valid week for someone else, then removing it is not possible,
  meaning that this assignment is not possible.

  If the assignment is made, propagate it to everyone else.

  If the assignment is not possible, return nil, else return the
  fully modified plan."
  [curr-plan person-name week]
  ;; Check hard and soft leave constraints against allocation For hard
  ;; leaves, assignment is not possible, return nil. For soft leaves
  ;; where other options are possible, assignment is not advised.
  (when-not (or (hard-leave-constraint? curr-plan person-name week)
                (soft-leave-constraint? curr-plan person-name week)
                (already-eliminated? curr-plan person-name week))
    ;; Assign the week to this person and eliminate the week for
    ;; everyone else.
    (-> curr-plan
        (assoc-in [person-name :next] #{week})
        (eliminate-week-for-others person-name week))))

(defn next-rotation
  [rotation]
  (let [urot (uniquify-rotation-entries rotation)
        [base-plan weeks-to-assign] (fill-base-values urot)]
    base-plan
    ;; (assign-week base-plan (:name (first urot)) (first weeks-to-assign))
    ))

(ns oncall
  "Given one oncall rotation schedule, generate the next one.

  The rotation is expected to be sorted in time (on prev-rotation-week
  value). Since weeks wrap over from 52 to 1, we don't want to sort
  ourselves. We expect the input to take care of this.

  The default rotation is stored in the resources file
  `resources/rotation.edn`.

  Re: duplicate entries. This list can contain duplicate entries (for
  simple book-keeping). However, the code only considers the latest
  entry for any person (unique on name)."
  (:require [clojure.set :as cset]))

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
                     constraints :constraints}]
  (let [[soft-constraints hard-constraints]
        (reduce (fn [[sc hc] [cweek ctype _]]
                  (if (= ctype :soft)
                    [(conj sc cweek) hc]
                    [sc (conj hc cweek)]))
                [#{} #{}]
                constraints)]
    (assoc plan
           person-name (merge {:next (if (seq hard-constraints)
                                       ;; Remove hard constraints here itself.
                                       (cset/difference next-values hard-constraints)
                                       next-values)}
                              (when prev-rotation
                                {:farthest-from prev-rotation})
                              (when (seq soft-constraints)
                                {:soft-constraints soft-constraints})
                              (when (seq hard-constraints)
                                {:hard-constraints hard-constraints})))))

(declare assign-week eliminate-week-for-others eliminate-week)

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
        weeks-to-assign (if (> 52 (+ week-counter num-weeks))
                          (range week-counter
                                 (+ week-counter num-weeks))
                          (concat (range week-counter 53)
                                  (range 1 (- (+ week-counter num-weeks) 52))))
        base-plan (reduce (partial add-person-to-plan (set weeks-to-assign))
                          {}
                          unique-rotation)
        base-plan-with-eliminations
        (reduce (fn [new-plan [pname pval]]
                  (if (second (:next pval))
                    new-plan
                    ;; Only a single value is possible for this
                    ;; person, eliminate this value for everyone else.
                    (if-let [plan (eliminate-week-for-others new-plan
                                                             pname
                                                             (first (:next pval)))]
                      plan
                      ;; no plan in possible
                      (reduced nil))))
                base-plan
                base-plan)]

    [base-plan-with-eliminations weeks-to-assign #{}]))

(defn hard-leave-constraint?
  "Is this allocation hitting a hard constraint? If so, return true."
  [curr-plan person-name week]
  (and (get-in curr-plan [person-name :hard-constraints])
       ((get-in curr-plan [person-name :hard-constraints]) week)))

(defn soft-leave-constraint?
  "Is this allocation hitting a soft constraint? If so, return true."
  [curr-plan person-name week]
  (and (get-in curr-plan [person-name :soft-constraints])
       ((get-in curr-plan [person-name :soft-constraints]) week)
       ;; other assignment options are currently possible
       (> (count (get-in curr-plan [person-name :next])) 1)))

(defn already-eliminated?
  "Return true if this allocation is already eliminated for the person."
  [curr-plan person-name week]
  (not ((get-in curr-plan [person-name :next]) week)))

(defn already-assigned?
  "Return true if this person only has a single week in their next
  slot (meaning that allocation is already done for the person)."
  [curr-plan person-name]
  (= 1 (count (get-in curr-plan [person-name :next]))))

(defn eliminate-week
  "Assumes that the week can be eliminated for the given person. If
  this leads to the person having only a single week left, this
  information is further propagated in the plan."
  [plan person-name week]
  (println "Eliminate Week: ")
  (println plan)
  (println person-name)
  (println week)
  (println "========")
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

(defn eliminate-week-for-others
  "Update the plan to eliminate the week for others.

  If this leaves anyone with only one other option, propagate that
  option into the plan as well."
  [curr-plan person-name week]
  (println "Eliminate for Others: ")
  (println curr-plan)
  (println person-name)
  (println week)
  (println "=======")
  (reduce (fn [new-plan [pname {possible-vals :next
                               :as pval}]]
            (if (= pname person-name)
              ;; Do nothing in this reduction, we are
              ;; trying to assign this week to this person
              ;; and using this reduction to eliminate the
              ;; week from everyone else.
              new-plan
              ;; Eliminate this week for everyone else.
              (if-let [new-plan (eliminate-week new-plan pname week)]
                new-plan
                (reduced nil))))
          curr-plan
          curr-plan))

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
  ;; Check hard and soft constraints against allocation. For hard
  ;; constraints, assignment is not possible, return nil. For soft
  ;; constraints where other options are possible, assignment is not
  ;; advised.
  (println "Assign Week: ")
  (println curr-plan)
  (println person-name)
  (println week)
  (println "=======")
  (when-not (or ;; No need to check for hard constraints, as those are
                ;; already eliminated when filling base values.
             (hard-leave-constraint? curr-plan person-name week)
             (soft-leave-constraint? curr-plan person-name week)
             (already-eliminated? curr-plan person-name week))
    ;; Eliminate the week for everyone else and then assign the week
    ;; to this person.
    (some-> curr-plan
            (eliminate-week-for-others person-name week)
            (assoc-in [person-name :next] #{week}))))

(defn swapper
  "Helper function to rotate my weeks vector.
  Use-case: The weeks vector is sorted in the order in which it should
  assign weeks. If a given week is not possible for someone, the
  immediate next week should be tried, and this week should be moved
  into the next position (effectively dropping it for this person, but
  keeping it the first week for the next person)"
  [swap-count coll]
  (let [drop-seq (drop swap-count coll)
        take-seq (take swap-count coll)
        head (take 1 drop-seq)]
    (lazy-cat head take-seq (drop 1 drop-seq))))

(defn next-rotation
  [rotation]
  (let [urot (uniquify-rotation-entries rotation)
        [base-plan weeks-to-assign assigned-weeks] (fill-base-values urot)]
    (loop [curr-plan base-plan
           names (map :name urot)
           weeks weeks-to-assign
           swap-counter 1
           loop-counter (* 2 (count urot))]
      (println "Next Rot Loop")
      (println curr-plan)
      (println names)
      (println weeks)
      (println swap-counter)
      (println loop-counter)
      (println "=======")
      (when (= 0 loop-counter)
        (throw (ex-info "Exiting this thing"
                        {:plan curr-plan
                         :names names
                         :weeks weeks})))
      (cond
        ;; Everyone has been assigned a week
        (empty? names) curr-plan

        ;; All available weeks have been assigned
        (empty? weeks) curr-plan

        ;; Some week is already assigned to this person
        (already-assigned? curr-plan (first names))
        (recur curr-plan (rest names) weeks 1 (dec loop-counter))

        :else
        (if-let [new-plan (assign-week curr-plan (first names) (first weeks))]
          ;; Assignment was successful, move to the next assignment.
          (recur new-plan (rest names) (rest weeks) 1 (dec loop-counter))
          ;; Assignment was unsuccessful, swap weeks and try again.
          (recur curr-plan
                 names
                 (swapper swap-counter weeks)
                 (inc swap-counter)
                 (dec loop-counter)))))))

;;; How to run this program
(comment
  (require '[oncall-util :as ou])
  (def rotation-file "resources/rotation.edn")
  (-> rotation-file
      ou/read-rotation
      next-rotation
      ou/display-plan))

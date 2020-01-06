(ns oncall
  "Given one oncall rotation schedule, generate the next one."
  (:require [clojure.edn :as edn]
            [java-time :as jt])
  (:import java.time.temporal.IsoFields))

(def rotation-file "resources/rotation.edn")

(defn read-rotation
  [rot-file]
  (edn/read-string (slurp rot-file)))

(def prev-rotation
  "The rotation is expected to be sorted in time (on
  prev-rotation-week value). Since weeks wrap over from 52 to 1, we
  don't want to sort ourselves. We expect the input to take care of
  this.

  Re: duplicate entries. This list can contain duplicate entries (for
  simple book-keeping). However, the code only considers the latest
  entry for any person (unique on name)."
  (read-rotation rotation-file))

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
           person-name (merge {:next next-values}
                              (when prev-rotation
                                {:farthest-from prev-rotation})
                              (when (seq soft-constraints)
                                {:soft-constraints soft-constraints})
                              (when (seq hard-constraints)
                                {:hard-constraints hard-constraints})))))

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
        weeks-to-assign (range week-counter
                               (+ week-counter num-weeks))
        base-plan (reduce (partial add-person-to-plan (set weeks-to-assign))
                          {}
                          unique-rotation)]

    [base-plan weeks-to-assign #{}]))

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
                         (if-let [new-plan (eliminate-week new-plan pname week)]
                           new-plan
                           (reduced nil))))
                     curr-plan
                     curr-plan)]
    ;; This assignment is possible. Make it and return the plan.
    (assoc-in updated-plan [person-name :next] #{week})))

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
  (when-not (or (hard-leave-constraint? curr-plan person-name week)
                (soft-leave-constraint? curr-plan person-name week)
                (already-eliminated? curr-plan person-name week))
    ;; Assign the week to this person and eliminate the week for
    ;; everyone else.
    (assign-week+eliminate-week-for-others curr-plan person-name week)))

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
           swap-count 1]
      (cond
        ;; Everyone has been assigned a week
        (empty? names) curr-plan

        ;; All available weeks have been assigned
        (empty? weeks) curr-plan

        ;; Some week is already assigned to this person
        (already-assigned? curr-plan (first names))
        (recur curr-plan (rest names) weeks 1)

        :else
        (if-let [new-plan (assign-week curr-plan (first names) (first weeks))]
          ;; Assignment was successful, move to the next assignment.
          (recur new-plan (rest names) (rest weeks) 1)
          ;; Assignment was unsuccessful, try the next week.
          (recur curr-plan
                 names
                 (swapper swap-count weeks)
                 (inc swap-count)))))))

;;; ========= DISPLAY Functions ==========

(defn week->date
  "Given a week, convert it to a date representation (Monday to
  Monday)."
  [week-num]
  (assert (>= 52 week-num 1)
          "Valid value for week is between 1 and 52")
  (let [start-date (.with (jt/local-date)
                          IsoFields/WEEK_OF_WEEK_BASED_YEAR
                          week-num)
        end-date (jt/plus start-date (jt/days 7))]
    (str (jt/format "<yyyy-MM-dd EEE>" start-date)
         "--"
         (jt/format "<yyyy-MM-dd EEE>" end-date))))

(defn sort-plan
  "Return a sorted array of entries in the plan, sorted on week-number
  to be assigned to someone."
  ;; @TODO: handle wrap-around
  [plan]
  (sort-by (comp first :next second)
           plan))

(defn display-plan
  "A function to take a plan and to render it in a human-readable format"
  [plan]
  (map (fn [[k v]]
         [k (-> v :next first week->date)])
       (sort-plan plan)))

;;; =========== STORE Functions ============

(defn plan-entry->file-entry
  [[pname pval]]
  {:name pname
   :in-next-rotation? true
   :prev-rotation-week (first (:next pval))
   :constraints []})

(defn plan->rot-file-fmt
  [plan rot-file]
  (let [existing-data (read-rotation rot-file)
        new-data (mapv plan-entry->file-entry (sort-plan plan))]
    (vec (concat existing-data new-data))))

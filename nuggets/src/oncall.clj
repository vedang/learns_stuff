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

;; Automatically added requires by core.typed
(require '[clojure.spec.alpha :as s])
;; Start: Generated by clojure.core.typed - DO NOT EDIT
(s/def
  ::ConstraintsInNextRotation?NameMap
  (s/keys
    :req-un
    [::constraints ::in-next-rotation? ::name]
    :opt-un
    [::assist ::prev-rotation-week]))
(s/def
  ::FarthestFromHardConstraintsNextMap
  (s/keys
    :req-un
    [::farthest-from ::hard-constraints ::next ::soft-constraints]))
(s/def
  ::NextFarthestFromHardConstraintsMap
  (s/keys
    :req-un
    [::next]
    :opt-un
    [::farthest-from ::hard-constraints ::soft-constraints]))
(s/def
  ::alias__0__0__0
  (s/nilable
    (s/tuple
      (s/map-of
        any?
        (s/or
          :nex-sof-har-far-tmp-HMap-alias36440
          ::nex-sof-har-far-tmp-HMap-alias36440
          :nex-far-sof-har-tmp-HMap-alias36520
          ::nex-far-sof-har-tmp-HMap-alias36520))
      (s/coll-of int?))))
(s/def
  ::alias__0__0__0__0__0__0__0__0__0__0
  (s/nilable
    (s/map-of
      any?
      (s/or
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520))))
(s/def
  ::alias__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0
  (s/nilable
    (s/map-of
      any?
      (s/or
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520))))
(s/def
  ::alias__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0
  (s/nilable
    (s/map-of
      any?
      (s/or
        :FarthestFromHardConstraintsNextMap
        ::FarthestFromHardConstraintsNextMap
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520))))
(s/def
  ::nex-far-sof-har-tmp-HMap-alias36520
  (s/or
    :spec-0
    (s/keys
      :req-un
      [::next ::soft-constraints]
      :opt-un
      [::farthest-from ::hard-constraints])
    :spec-1
    (s/keys
      :req-un
      [::next]
      :opt-un
      [::farthest-from ::hard-constraints ::soft-constraints])))
(s/def
  ::nex-sof-har-far-tmp-HMap-alias36440
  (s/or
    :spec-0
    (s/keys
      :req-un
      [::next ::soft-constraints]
      :opt-un
      [::farthest-from ::hard-constraints])
    :spec-1
    (s/keys
      :req-un
      [::next]
      :opt-un
      [::farthest-from ::hard-constraints ::soft-constraints])))
(s/def ::assist string?)
(s/def
  ::constraints
  (s/coll-of (s/tuple int? #{:soft :hard} #{:leave}) :into vector?))
(s/def ::farthest-from int?)
(s/def ::hard-constraints set?)
(s/def ::in-next-rotation? boolean?)
(s/def ::name string?)
(s/def ::next set?)
(s/def ::prev-rotation-week int?)
(s/def ::soft-constraints set?)
(s/fdef
  add-person-to-plan
  :args
  (s/cat
    :next-values
    set?
    :plan
    (s/map-of any? ::NextFarthestFromHardConstraintsMap)
    :arg-2
    ::ConstraintsInNextRotation?NameMap)
  :ret
  (s/map-of
    any?
    (s/or
      :nex-sof-har-far-tmp-HMap-alias36440
      ::nex-sof-har-far-tmp-HMap-alias36440
      :nex-far-sof-har-tmp-HMap-alias36520
      ::nex-far-sof-har-tmp-HMap-alias36520)))
(s/fdef
  already-assigned?
  :args
  (s/cat
    :curr-plan
    (s/map-of
      string?
      (s/or
        :FarthestFromHardConstraintsNextMap
        ::FarthestFromHardConstraintsNextMap
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :person-name
    string?)
  :ret
  boolean?)
(s/fdef
  already-eliminated?
  :args
  (s/cat
    :curr-plan
    (s/map-of
      string?
      (s/or
        :FarthestFromHardConstraintsNextMap
        ::FarthestFromHardConstraintsNextMap
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :person-name
    string?
    :week
    int?)
  :ret
  boolean?)
(s/fdef
  assign-week
  :args
  (s/cat
    :curr-plan
    (s/map-of
      string?
      (s/or
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :person-name
    string?
    :week
    int?)
  :ret
  ::alias__0__0__0__0__0__0__0__0__0__0)
(s/fdef
  debug!
  :args
  (s/cat :args (s/* (s/or :string? string? :int? int?)))
  :ret
  nil?)
(s/fdef
  eliminate-week
  :args
  (s/cat
    :plan
    (s/map-of
      string?
      (s/or
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :person-name
    string?
    :week
    int?)
  :ret
  ::alias__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0)
(s/fdef
  eliminate-week-for-others
  :args
  (s/cat
    :curr-plan
    (s/map-of
      any?
      (s/or
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :person-name
    string?
    :week
    int?)
  :ret
  ::alias__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0__0)
(s/fdef
  fill-base-values
  :args
  (s/cat
    :unique-rotation
    (s/coll-of ::ConstraintsInNextRotation?NameMap :into vector?))
  :ret
  (s/tuple
    (s/map-of
      any?
      (s/or
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520))
    (s/coll-of int?)))
(s/fdef
  generate-plan
  :args
  (s/alt
    :4-args
    (s/cat
      :plan
      (s/map-of
        any?
        (s/or
          :nex-sof-har-far-tmp-HMap-alias36440
          ::nex-sof-har-far-tmp-HMap-alias36440
          :nex-far-sof-har-tmp-HMap-alias36520
          ::nex-far-sof-har-tmp-HMap-alias36520))
      :names
      (s/coll-of string?)
      :weeks
      (s/coll-of int?)
      :num-attempts
      int?)
    :5-args
    (s/cat
      :plan
      (s/map-of
        any?
        (s/or
          :nex-sof-har-far-tmp-HMap-alias36440
          ::nex-sof-har-far-tmp-HMap-alias36440
          :nex-far-sof-har-tmp-HMap-alias36520
          ::nex-far-sof-har-tmp-HMap-alias36520))
      :names
      (s/coll-of string?)
      :weeks
      (s/coll-of int?)
      :num-attempts
      int?
      :swapper-count
      int?))
  :ret
  any?)
(s/fdef
  hard-leave-constraint?
  :args
  (s/cat
    :curr-plan
    (s/map-of
      string?
      (s/or
        :FarthestFromHardConstraintsNextMap
        ::FarthestFromHardConstraintsNextMap
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :person-name
    string?
    :week
    int?)
  :ret
  (s/nilable int?))
(s/fdef
  next-rotation
  :args
  (s/cat
    :rotation
    (s/coll-of ::ConstraintsInNextRotation?NameMap :into vector?))
  :ret
  any?)
(s/fdef
  optimize-base-values
  :args
  (s/cat
    :base-plan
    (s/map-of
      string?
      (s/or
        :nex-sof-har-far-tmp-HMap-alias36440
        ::nex-sof-har-far-tmp-HMap-alias36440
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :weeks-to-assign
    (s/coll-of int?))
  :ret
  ::alias__0__0__0)
(s/fdef
  soft-leave-constraint?
  :args
  (s/cat
    :curr-plan
    (s/map-of
      string?
      (s/or
        :nex-far-sof-har-tmp-HMap-alias36520
        ::nex-far-sof-har-tmp-HMap-alias36520
        :NextFarthestFromHardConstraintsMap
        ::NextFarthestFromHardConstraintsMap))
    :person-name
    string?
    :week
    int?)
  :ret
  (s/nilable boolean?))
(s/fdef
  swapper
  :args
  (s/cat :swap-count int? :coll (s/coll-of int?))
  :ret
  (s/coll-of int?))
(s/fdef
  uniquify-rotation-entries
  :args
  (s/cat
    :rotation
    (s/coll-of ::ConstraintsInNextRotation?NameMap :into vector?))
  :ret
  (s/coll-of ::ConstraintsInNextRotation?NameMap :into vector?))
;; End: Generated by clojure.core.typed - DO NOT EDIT
(def ^:dynamic *debug-flag*
  "Flag to print debug information"
  nil)

(defn debug!
  [& args]
  (when *debug-flag*
    (doseq [a args]
      (println a))
    (println "=======")))

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
  person to the plan. Remove any hard constraints from the set of
  possibilities here itself, if a direct assignment is made, make it
  here itself."
  [next-values plan {person-name :name
                     prev-rotation :prev-rotation-week
                     constraints :constraints
                     fixed-next :manually-set-next-rotation}]
  (let [[soft-constraints hard-constraints]
        (reduce (fn [[sc hc] [cweek ctype _]]
                  (if (= ctype :soft)
                    [(conj sc cweek) hc]
                    [sc (conj hc cweek)]))
                [#{} #{}]
                constraints)
        ;; For any person, try and keep the next rotation this far
        ;; away from the current rotation.
        ideal-distance (count next-values)
        ;; For this person, does the week-num wrap around? Eg: if
        ;; `prev-rotation` was 40 and `ideal-distance` is 20, then the
        ;; ideal next week is 8 (in the next year). This is a case of
        ;; wrap around.
        week-wrap-around? (> ((fnil + 0) prev-rotation ideal-distance) 52)
        person-details
        (merge {:next (cond
                        ;; We have a pre-decided week for this person
                        fixed-next #{fixed-next}
                        ;; Remove hard constraints here itself.
                        (seq hard-constraints)
                        (cset/difference next-values hard-constraints)
                        ;; All given possibilities are possible
                        :else next-values)
                :ideal-distance ideal-distance
                :week-wrap-around? week-wrap-around?}
               (when prev-rotation
                 {:prev prev-rotation
                  :ideal-week (if (> (+ prev-rotation ideal-distance) 52)
                                (- (+ prev-rotation ideal-distance) 52)
                                (+ prev-rotation ideal-distance))})
               (when (seq soft-constraints)
                 {:soft-constraints soft-constraints})
               (when (seq hard-constraints)
                 {:hard-constraints hard-constraints}))]
    (assoc plan person-name person-details)))

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
                          unique-rotation)]
    [base-plan weeks-to-assign]))

(defn optimize-base-values
  "When we fill base values:

  1) The base plan itself might be invalid. (eg: some person has hard
  constraints against every week, and therefore the `next` set is
  empty.

  2) The base plan might have weeks already assigned to certain
  people. eg: hard constraints only leave a single choice, or we've
  predetermined a week for someone manually. In this case, we need to
  eliminate this option from other folks possible values and from
  weeks we will attempt to assign.

  Take care of these conditions. Return the `optimized-plan`,
  `names-needing-assignment` and `weeks-to-assign`, or `nil` if the
  given configuration is invalid."
  [base-plan names-needing-assignment weeks-to-assign]
  (debug! "Optimize Base Values:" base-plan names-needing-assignment weeks-to-assign)
  (when-let [opt-plan
             (reduce (fn [new-plan [pname pval]]
                       (cond
                         ;; More than one possible value exists for the person
                         (second (:next pval))
                         new-plan
                         ;; Only a single value is possible for this
                         ;; person, eliminate this value for everyone else.
                         (first (:next pval))
                         (if-let [plan (eliminate-week-for-others new-plan
                                                                  pname
                                                                  (first (:next pval)))]
                           plan
                           ;; no plan in possible
                           (reduced nil))
                         ;; No value exists for this person, this plan is not possible
                         :else (reduced nil)))
                     base-plan
                     base-plan)]
    (let [[opt-name-set opt-week-set]
          (reduce (fn [[nns ws] [pname pval]]
                    (if (second (:next pval))
                      ;; nothing to eliminate from our lists
                      [nns ws]
                      [(disj nns pname) (disj ws (first (:next pval)))]))
                  [(set names-needing-assignment) (set weeks-to-assign)]
                  opt-plan)]
      [opt-plan
       (filter opt-name-set names-needing-assignment)
       (filter opt-week-set weeks-to-assign)])))

(defn soft-leave-constraint?
  "Is this allocation hitting a soft constraint? If so, return true."
  [plan person-name week]
  (and (get-in plan [person-name :soft-constraints])
       ((get-in plan [person-name :soft-constraints]) week)
       ;; other assignment options are currently possible
       (> (count (get-in plan [person-name :next])) 1)))

(defn already-eliminated?
  "Return true if this allocation is already eliminated for the person."
  [plan person-name week]
  (not ((get-in plan [person-name :next]) week)))

(defn already-assigned?
  "Return true if this person only has a single week in their next
  slot (meaning that allocation is already done for the person)."
  [plan person-name]
  (= 1 (count (get-in plan [person-name :next]))))

(defn get-assigned-week
  "Return the week-number if this person only has a single week in their
  next slot (meaning that allocation is already done for the person).
  Else return nil."
  [plan person-name]
  (when (= 1 (count (get-in plan [person-name :next])))
    (first (get-in plan [person-name :next]))))

(defn eliminate-week
  "Assumes that the week can be eliminated for the given person. If
  this leads to the person having only a single week left, this
  information is further propagated in the plan."
  [plan person-name week]
  (debug! "Eliminate Week:" plan person-name week)
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
  "For every person other than the one specifically mentioned, try and
  eliminate the week from the given plan.

  If this leaves anyone with only one other option, propagate that
  option into the plan as well."
  [plan person-name week]
  (debug! "Eliminate for Others:" plan person-name week)
  (reduce (fn [new-plan [pname _]]
            (or (eliminate-week new-plan pname week)
                (reduced nil)))
          plan
          (dissoc plan person-name)))

(defn assign-week
  "Trying to assign a week to a person means the following:

  1. Check if this is a valid assignment (the next vector contains
  this week).

  2. Check if there is a soft constraint against this person for this
  week. In this case, if there are other options available, consider
  the assignment as not possible.

  4. Remove that week as a possibility for everyone else. If it's the
  last valid week for someone else, then removing it is not possible,
  meaning that this assignment is not possible.

  If the assignment is not possible, return nil, else return the
  fully modified plan."
  [plan person-name week & {:keys [ignore-soft-constraint?]}]
  (debug! "Assign Week:" plan person-name week)
  (when-not (or ;; No need to check for hard constraints, as
                ;; those are already eliminated when filling base
                ;; values.
             (and (not ignore-soft-constraint?)
                  (soft-leave-constraint? plan person-name week))
             (already-eliminated? plan person-name week))
    ;; Eliminate the week for everyone else and then assign the week
    ;; to this person.
    (some-> plan
            (eliminate-week-for-others person-name week)
            (assoc-in [person-name :next] #{week}))))

(defn generate-plan
  "Given an optimized `base-plan`, a list of people-names and a list
  of weeks, return the oncall schedule. Takes a `num-attempts` counter
  to determine how many times to attempt generating the plan before
  giving up. If not provided, it is equal to the number of people we
  are trying to assign a schedule to. On giving up, throws an
  exception."
  [plan names weeks]
  (debug! "Generate Plan:" plan names weeks)
  (cond
    ;; Everyone has been assigned a week, return the plan.
    (empty? names) plan

    ;; No weeks left to assign, return the plan.
    (empty? weeks) plan

    ;; Some week is already assigned to this person. Remove this week
    ;; from the list of weeks we are trying to assign to other
    ;; people.
    (already-assigned? plan (first names))
    (let [assigned-week (get-assigned-week plan (first names))]
      (recur plan (rest names) (remove #{assigned-week} weeks)))

    :else
    (if-let [new-plan (assign-week plan (first names) (first weeks))]
      ;; Assignment was successful, move to the next assignment.
      (recur new-plan (rest names) (rest weeks))
      ;; Assignment was unsuccessful, drop a week and try again.
      (let [new-plan (generate-plan plan names (rest weeks))]
        ;; Here there will be a new plan returned with a successful
        ;; assignment for the affected person, or if there is no
        ;; successful assignment, there is nothing we can do for this
        ;; person via constraint propagation. Leave the `weeks` list
        ;; as it is, but drop the person from further checks.
        (recur new-plan (rest names) weeks)))))

(defn best-week
  "Given all the information of the person, choose the best week from
  the possible set of weeks to attempt assignment."
  [{:keys [prev ideal-distance ideal-week week-wrap-around? next]
    :as pval}]
  (debug! "Best Week:" pval)
  (if ideal-week
    ;; Choose the option closest to the ideal-week for this person
    (let [wrap-week? (if week-wrap-around?
                       ;; set of weeks where distance needs to be
                       ;; calculated differently.
                       (set (range (inc prev) 53))
                       #{})
          week-distances (map (fn [w]
                                (if (wrap-week? w)
                                  [w (+ ideal-week (- 52 w))]
                                  [w (Math/abs (- ideal-week w))]))
                              next)]
      (first (first (sort-by second week-distances))))
    ;; Choose randomly.
    (first next)))

(defn solve-by-search
  "For the given `plan`, if a person does not have a week assigned to
  them, solve the plan by choosing a value from their possible next
  values and propagating eliminations."
  [plan ordered-names]
  (debug! "Solve by search:" plan ordered-names)
  (let [solved-predfn (comp (partial = 1) count :next val)]
    (if (every? solved-predfn plan)
      ;; Solved!
      plan
      (let [pending-plan (->> plan
                              (sort-by (comp count :next val))
                              (drop-while solved-predfn)
                              (mapcat identity)
                              (apply hash-map))
            pending-names (set (keys pending-plan))
            first-name (some pending-names ordered-names)
            week-to-attempt (best-week (pending-plan first-name))]
        (or (some-> plan
                    (assign-week first-name week-to-attempt :ignore-soft-constraint? true)
                    (solve-by-search ordered-names))
            (some-> plan
                    (eliminate-week first-name week-to-attempt)
                    (solve-by-search ordered-names))
            (do (println "No search plan is possible beyond the given constraints. Printing final-plan and exiting.")
                plan))))))

(defn next-rotation
  [rotation]
  (let [urot (uniquify-rotation-entries rotation)
        [base-plan weeks-to-assign] (fill-base-values urot)]
    (if-let [ret (optimize-base-values base-plan
                                       (map :name urot)
                                       weeks-to-assign)]
      ;; `optimize-base-values` returns a tuple of `starting-plan`,
      ;; `names-needing-assignment` and `weeks-needing-assignment`.
      (let [constraint-plan (generate-plan (first ret) (second ret) (last ret))]
        (solve-by-search constraint-plan (map :name urot)))
      ;; No plan is possible.
      (do (println "No plan is possible for the given constraints. Printing base-plan and exiting.")
          base-plan))))

;;; How to run this program
(comment
  (require '[oncall-util :as ou])
  (def rotation-file "resources/rotation.edn")
  (-> rotation-file
      ou/read-rotation
      next-rotation
      ou/display-plan)
  ;; If you want to see what is happening.
  (alter-var-root #'*debug-flag* (constantly true))
  ;; To generate new data for the rotation file (which you will need
  ;; for the next time you want to get a new schedule)
  (-> rotation-file
      ou/read-rotation
      next-rotation
      (ou/plan->rot-file-fmt rotation-file)))

(ns challenges.psk
  "A Simulation of the Passport Seva Kendra"
  (:require [clj-time.core :as ct]
            [clojure.tools.logging :as ctl])
  (:import java.util.concurrent.LinkedBlockingQueue))

(def working-hours? (atom false)) ; use this to control agents.
                                        ; Turning this off will
                                        ; shutdown agents.

(def stages
  "The various stages in the PSK, and transition from one stage to the other.

  - `processing-time-range` represents the amount of time spent at the
  counter in this stage.
  - `next` represents the next stage for the person.
  - `counters` represent the number of counters/agents serving this stage."
  {::enter {:next ::doc-verification}
   ::doc-verification {:next ::biometrics
                       :counters 10
                       :display-str "A-"
                       :processing-time-range [5 10]}
   ::biometrics {:next ::form-check
                 :counters 33
                 :display-str "B-"
                 :processing-time-range [3 15]}
   ::form-check {:next ::final-check
                 :counters 12
                 :display-str "C-"
                 :processing-time-range [2 4]
                 :failure ::corrections}
   ::final-check {:next ::exit
                  :counters 10
                  :display-str "D-"
                  :processing-time-range [2 4]}
   ::corrections {:next ::form-check
                  :counters 3
                  :display-str "E-"
                  :processing-time-range [5 15]}})

(def waiting-room-capacity 500) ; max number of waiting people

(def total-capacity
  "number of people that can be in the PSK at max"
  (apply + waiting-room-capacity (keep :counters (vals stages))))

(def processing-batch-size
  "no of people entering the center at one time."
  25)

(def new-batch-in-mins
  "time between one batch and the next"
  15)

(def guide-people-to-next-stage-ms 500) ; every x millis, guide people
                                        ; to the next stage.

(def stage-status
  "For the given stage, the map of possible states that a person can be
  in."
  [;; Person is in the waiting area, looking at the display to see
   ;; when he's called to a counter.
   ::waiting
   ;; Counter is assigned to the person
   ::in-process
   ;; Person is done with the counter and heading to the next stage.
   ::done])

(def token-generator
  "Give the next token number to the applicant."
  (atom {:normal 0
         :senior 0
         :tatkal 0
         :police-clearance 0}))

(defrecord Token [type val])

(def ^:private token-type-distribution
  "Represents which % of population falls under which passport type.

  This distribution is just my guess! :P"
  {:normal 80
   :senior 10
   :tatkal 8
   :police-clearance 2})

(defn- get-token-type
  "Returns a random token type, skewed by weights defined above."
  []
  (let [[total weighted-seq]
        (reduce (fn [[n ws] m]
                  [(+ n (:dist m))
                   (conj ws
                         (assoc m :weight (+ n (:dist m))))])
                [0 []]
                (sort-by :dist
                         (map (fn [[k v]] {:type k :dist v})
                              token-type-distribution)))
        w (rand-int total)]
    (loop [elem (first weighted-seq)
           ws (rest weighted-seq)]
      (if (<= w (:weight elem))
        (:type elem)
        (recur (first ws) (rest ws))))))

;;; `enter-time` and `exit-time` are vanity metrics to
;;; calculate the amount of time the person spent in the PSK.
(defrecord Person
    [token stage stage-status enter-time exit-time total-time psk-agent])

(let [token-type->char {:normal "N-"
                        :senior "S-"
                        :tatkal "T-"
                        :police-clearance "P-"}]
  (defn- person-representation
    "The visual representation of the applicant. eg: N-300, T-50 etc."
    [person]
    (let [token (:token person)]
      (str (token-type->char (:type token)) (:val token)))))

(defn create-person!
  "Create a new person for the PSK. Token numbers are always
  monotonically increasing. This function modifies the global atom
  `token-generator` as a side-effect. This atom is used to generate
  the next token number."
  []
  (let [person-type (get-token-type)
        person-number (get (swap! token-generator
                                  update
                                  person-type
                                  inc)
                           person-type)]
    (Person. (Token. person-type person-number)
             ::enter
             ::done
             (ct/now)
             nil
             nil
             nil)))

(defn- get-processing-time-for-stage
  "Given a `stage-config`, get the processing time range for the stage.
  Calculate a random amount of time within this range. Note that we
  treat mins as secs in this simulation."
  [stage-config]
  (let [[min-time max-time] (:processing-time-range stage-config)]
    (* 1000 (+ min-time (rand-int (- max-time min-time))))))

(defn- agent-representation
  "The visual representation of the psk-agent. eg: A-300, B-50 etc."
  [psk-agent]
  (str (get-in psk-agent [:config :display-str]) (:id psk-agent)))

(defrecord PSKAgent
    [id type config])

(defn- create-agents
  "For the given `agent-type`, create the given `num` of agents."
  [agent-type stage-config num]
  (map (comp agent (fn [i] (PSKAgent. (inc i) agent-type stage-config)))
       (range num)))

(defn create-kendra-agents
  "Given the `stages` and their config for the kendra, create the
  appropriate agents to work these counters."
  [kendra-stages]
  (let [stages-with-counters (-> kendra-stages
                                 keys
                                 set
                                 ;; Remove the stages where no counter
                                 ;; of agents is needed.
                                 (disj ::enter ::exit))]
    (map (fn [s]
           (let [config (get kendra-stages s)]
             (create-agents s config (:counters config))))
         stages-with-counters)))

(defn- call-person-to-counter
  "Announce that person should come to the processing counter. Takes
  `person` and `notice-board` refs, performs a transactional update."
  [stage notice-board psk-agent person]
  (dosync
   (alter person
          assoc
          :stage stage
          :stage-status ::in-process
          :psk-agent (agent-representation psk-agent))
   (alter notice-board
          assoc
          (person-representation @person)
          (agent-representation psk-agent))))

(defn- process-person
  "Do the work for processing the given person. Takes a `person`
  object and not a ref."
  [stage stage-config psk-agent person]
  (let [processing-time (get-processing-time-for-stage stage-config)]
    (ctl/debug (format "[Agent: %s] [Stage: %s] Doing %s ms of work for %s person"
                       (agent-representation psk-agent)
                       stage
                       processing-time
                       (person-representation person)))
    (Thread/sleep processing-time)))

(defn- mark-processing-as-complete
  "For this `person`, this `stage` is complete. Update accordingly.
  Takes `person` and `notice-board` refs, performs a transactional
  update."
  [stage notice-board psk-agent person]
  (let [person-str (person-representation @person)]
    (dosync
     (alter person assoc :stage stage :stage-status ::done :psk-agent nil)
     (alter notice-board dissoc person-str))

    (ctl/debug "Processing Complete!"
               (assoc psk-agent :current-applicant person-str))))

(defn process-applicant
  "Get a person from the queue. Process this person as per the rules
  of the counter."
  [psk-agent my-queue notice-board]
  (ctl/debug (format "[Agent: %s] Waiting for next person!"
                     (agent-representation psk-agent)))
  (if @working-hours?

    (if-let [person (.poll my-queue 1 java.util.concurrent.TimeUnit/SECONDS)]
      (let [stage (:type psk-agent)
            stage-config (:config psk-agent)]
        (ctl/debug (format "[Agent: %s] We have a person: %s"
                           (agent-representation psk-agent)
                           person))
        (call-person-to-counter stage notice-board psk-agent person)
        (process-person stage stage-config psk-agent @person)
        (mark-processing-as-complete stage notice-board psk-agent person)
        ;; Next!
        (ctl/debug "Repeating Agent")
        (send-off *agent* process-applicant my-queue notice-board)
        ;; Set the new state of the agent.
        (assoc psk-agent :last-processed (person-representation @person)))
      (do (send-off *agent* process-applicant my-queue notice-board)
          psk-agent))

    (ctl/info (format "[Agent: %s] Working hours are over! Closing Shop! Come back later!"
                      (agent-representation psk-agent)))))

(defn- book-keeping-for-new-applicants
  "Remove all applicants who are completely done from
  `active-applicants`. Store them in `done-applicants` for
  book-keeping.

  *NOTE* : Since this goes through the entire collection, it is slow.
  Hence we run it when sending in new batches of people."
  [active-applicants done-applicants]
  (ctl/debug "Removing completed applications for book-keeping!")
  (dosync
   (let [[active-people done-people] (reduce (fn [[aa da] p]
                                               (if (and (= (:stage @p) ::exit)
                                                        (= (:stage-status @p) ::done))
                                                 [aa (conj da p)]
                                                 [(conj aa p) da]))
                                             [[] []]
                                             @active-applicants)]
     (ref-set active-applicants active-people)
     (alter done-applicants into done-people))))

(defn let-people-through
  "Send people into the PSK in batches as defined by
  `processing-batch-size` and `new-batch-in-mins`. Note that in our
  code we use seconds to represent minutes."
  [active-applicants done-applicants]
  (future
    (loop []
      (if @working-hours?

        (if (> (count @active-applicants)
               (- total-capacity processing-batch-size))

          (do (ctl/info "[Entry] PSK at max capacity! We need to cancel this batch!")
              (Thread/sleep (* 1000 new-batch-in-mins))
              (recur))

          (let [new-people (->> create-person!
                                (repeatedly processing-batch-size)
                                (map ref))]
            (dosync
             (alter active-applicants into new-people))
            (ctl/debug (format "[Entry] %s new people entered into the PSK."
                               (count new-people)))

            (book-keeping-for-new-applicants active-applicants done-applicants)

            (Thread/sleep (* 1000 new-batch-in-mins))
            (recur)))

        (ctl/info "[Entry] Working hours are over! Closing Shop! Come back later!")))))

(defn- mark-applicant-process-as-complete
  "Takes a `person` ref object and marks its processing as complete."
  [person]
  (let [time-instant (ct/now)]
    (dosync
     (alter person
            assoc
            :stage ::exit
            :exit-time time-instant
            :total-time (-> @person
                            :enter-time
                            (ct/interval time-instant)
                            ct/in-seconds))))

  (ctl/debug (format "Dear %s, Thank you for visiting the Passport Seva Kendra! Your entire experience took: %s mins!"
                     (person-representation @person)
                     (:total-time @person))))

(defn- move-applicant-to-next-stage
  "Given a `person` ref and the next stage they should go to, move
  them to the stage. Does a transactional update."
  [stage->queue next-stage person]
  (ctl/debug (format "[Guide] %s from %s to %s"
                     (person-representation @person)
                     (:stage @person)
                     next-stage))
  (dosync
   (alter person
          assoc
          :stage next-stage
          :stage-status ::waiting))
  (.offer (get stage->queue next-stage)
          person
          1
          java.util.concurrent.TimeUnit/SECONDS))

(defn move-people-through
  "Review all the active applicants and move them into appropriate stages."
  [kendra-stages stage->queue active-applicants]
  (future
    (loop []
      (if @working-hours?

        (let [people @active-applicants]
          (doseq [person (->> people
                              (group-by (comp :stage-status deref))
                              ::done)]
            (if (= ::exit (get-in kendra-stages [(:stage @person) :next]))
              (mark-applicant-process-as-complete person)
              (when-let [next-stage (get-in kendra-stages
                                            [(:stage @person) :next])]
                (move-applicant-to-next-stage stage->queue next-stage person))))

          (Thread/sleep guide-people-to-next-stage-ms)
          (recur))

        (ctl/info "[Guide] Working hours are over! Closing Shop! Come back later!")))))

(defn create-kendra-queues
  "Given the counter-types / `stages` in the kendra, create the
  appropriate queues."
  [kendra-stages q-capacity]
  (let [queues-we-need (-> kendra-stages
                           keys
                           set
                           ;; Remove the stages where no queue of
                           ;; people is needed.
                           (disj ::enter ::exit))]
    (reduce (fn [m s]
              (assoc m
                     s (LinkedBlockingQueue. q-capacity)))
            {}
            queues-we-need)))

(defn- show-applicant-info
  "Helper function to print overall applicant information."
  [applicants]
  (let [people @applicants]
    (reduce (fn [m [stage ps]]
              (assoc m
                     stage (merge (reduce (fn [m [st p]] (assoc m st (count p)))
                                          {}
                                          (group-by (comp :stage-status deref) ps))
                                  {::total (count ps)})))
            {:absolute-total (count people)}
            (group-by (comp :stage deref)
                      people))))

(defn- get-processing-time-info
  "Print interesting information about "
  [done-applicants]
  (let [header ["Total Processing Time (mins): "
                "Total People Processed: "
                "Avg Processing Time (mins): "
                "Max Processing Time (mins): "
                "Person who took the most time: "]
        numbers (reduce (fn [[tot n avg max max-name] p]
                          [(+ tot (:total-time @p))
                           (inc n)
                           (int (/ (+ tot (:total-time @p)) (inc n)))
                           (if (> max (:total-time @p))
                             max
                             (:total-time @p))
                           (if (> max (:total-time @p))
                             max-name
                             (person-representation @p))])
                        [0 0 0 0 ""]
                        @done-applicants)]
    (interleave header numbers)))

(defn start-the-kendra!
  "Setup our Passport Seva Kendra."
  []
  (let [;; Create queues for the various stages, returns a map of stage->queue
        stage->queue (create-kendra-queues stages total-capacity)
        ;; Create all the agents
        list-of-agents (create-kendra-agents stages)
        ;; create display board for waiting members
        notice-board (ref (sorted-map))
        ;; track all active applicants
        active-applicants (ref [])
        ;; track all applicants (for debugging / historical data purpose)
        done-applicants (ref [])]
    ;; for each agent at each counter, start processing!
    (ctl/info "[PSK] Welcome, today is a good day.")
    (doseq [as list-of-agents]
      (let [s (-> as first deref :type)
            q (get stage->queue s)]
        (doseq [a as]
          (send-off a process-applicant q notice-board))))
    (let-people-through active-applicants done-applicants)
    ;; start a helper process to move people from one stage to the other.
    (move-people-through stages stage->queue active-applicants)
    [notice-board active-applicants done-applicants]))


(comment
  (reset! working-hours? true)
  (def state (start-the-kendra!))
  (clojure.pprint/pprint @(first state))
  (clojure.pprint/pprint (show-applicant-info (second state)))
  (clojure.pprint/pprint (show-applicant-info (last state)))
  (clojure.pprint/pprint (get-processing-time-info (last state)))
  (reset! working-hours? false))

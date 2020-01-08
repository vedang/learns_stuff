(ns oncall-util
  (:require [clojure.edn :as edn]
            [java-time :as jt])
  (:import java.time.DayOfWeek
           [java.time.temporal IsoFields TemporalAdjusters]))

(defn read-rotation
  [rot-file]
  (edn/read-string (slurp rot-file)))

;;; ========= DISPLAY Functions ==========

(defn week->date
  "Given a week, convert it to a date representation (Monday to
  Monday)."
  [week-num]
  (assert (>= 52 week-num 1)
          "Valid value for week is between 1 and 52")
  (let [start-date (.. (jt/local-date)
                       (with IsoFields/WEEK_OF_WEEK_BASED_YEAR week-num)
                       (with (. TemporalAdjusters (previousOrSame DayOfWeek/MONDAY))))
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

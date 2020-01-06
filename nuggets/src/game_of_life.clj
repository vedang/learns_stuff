(ns game-of-life
  (:require [clojure.set :as cs]))

(defn calculate-neighbour-coordinates*
  [[x y]]
  ;; The order of generating the vector is important because it is
  ;; used in property-based testing of the function.
  (->> (vector [(- x 1) (- y 1)]
               [(- x 1) y]
               [(- x 1) (+ y 1)]
               [x (- y 1)]
               [x (+ y 1)]
               [(+ x 1) (- y 1)]
               [(+ x 1) y]
               [(+ x 1) (+ y 1)])
       (filter (fn [[x-ord y-ord]]
                 (and (>= x-ord 0) (>= y-ord 0))))
       set))

(def calculate-neighbour-coordinates
  "Given a cell, return the set of neighboring cells."
  (memoize calculate-neighbour-coordinates*))

(defn find-live-cells
  "Internal refactoring of common code to process cell health for a
  set of cells.

  Given a set of `cells-under-test`, the `current-live-cells` and a
  `fitness-pred`, returns cells matching the pred."
  [cells-under-test current-live-cells fitness-pred]
  (reduce (fn [healthy-cells cell]
            (let [neighbouring-cells (calculate-neighbour-coordinates cell)
                  live-neighbouring-cells (cs/intersection current-live-cells
                                                           neighbouring-cells)]
              (if (fitness-pred live-neighbouring-cells)
                (conj healthy-cells cell)
                healthy-cells)))
          #{}
          cells-under-test))

(defn find-revived-cells
  "Given the `current-dead-cells` and the `current-live-cells`, return
  the ones which revive in the next generation."
  [current-dead-cells current-live-cells]
  (find-live-cells current-dead-cells
                  current-live-cells
                  (fn [live-neighbouring-cells]
                    (= (count live-neighbouring-cells) 3))))

(defn find-surviving-cells
  "Given the `current-live-cells`, return the ones which survive in the
  next generation."
  [current-live-cells]
  (find-live-cells current-live-cells
                  current-live-cells
                  (fn [live-neighbouring-cells]
                    (< 1 (count live-neighbouring-cells) 4))))

(defn calculate-next-generation
  "Given the `current-live-cells` set, return the next generation of
  live cells."
  [current-live-cells]
  (let [all-neighbours (->> current-live-cells
                            (map calculate-neighbour-coordinates)
                            (apply cs/union))
        current-dead-cells (cs/difference all-neighbours current-live-cells)]
    (cs/union (find-surviving-cells current-live-cells)
              (find-revived-cells current-dead-cells current-live-cells))))

(comment
  ;; Take the initial state from the caller, start the game of life.
  ;; The initial state is a set of tuples. Each tuple represents the x,y
  ;; co-ordinates of a live cell.
  ;; eg: #{[0 1] [1 1] [1 2]}
  (calculate-next-generation initial-state))

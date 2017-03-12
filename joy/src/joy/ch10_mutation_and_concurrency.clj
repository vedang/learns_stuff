(ns joy.ch10-mutation-and-concurrency
  (:require [joy.ch05-collection-types :refer [neighbours]])
  (:import java.util.concurrent.Executors))

(def thread-pool
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads!
  [f & {thread-count :threads
        exec-count   :times
        :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit thread-pool
             #(dotimes [_ exec-count] (f)))))

(def initial-board
  ":k is the black king, and :K is the white king on a 3x3 board."
  [[:- :k :-]
   [:- :- :-]
   [:- :K :-]])

(defn board-map
  "Given a board, run the given function on every cell of the board."
  [f board]
  (vec (map (fn [row]
              (vec (for [pos row]
                     (f pos))))
            board)))

(defn reset-board!
  "Resets the board state.  Generally these types of functions are a
   bad idea, but matters of page count force our hand."
  []
  (def board (board-map ref initial-board))
  (def move-order-tuple
    "Starting point: K is at 2,1 and k at 0,1 in the matrix. K starts
    the game. (First member of the Tuple to play)"
    (ref [[:K [2 1]]
          [:k [0 1]]]))
  (def num-moves (ref 0)))

(def king-moves
  "King can move only from his cell to one of his horizontally /
  vertically / diagonally surrounding cells."
  (partial neighbours
           [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
           3))

(defn good-move?
  "A move is 'good' if the enemy isn't already on that square."
  [to enemy-sq]
  (when (not= to enemy-sq)
    to))

(defn choose-move
  "Randomly choose a legal move."
  [move]
  (let [[[mover mpos] [_ enemy-pos]] move]
    [mover (some #(good-move? % enemy-pos)
                 (shuffle (king-moves mpos)))]))

(defn place
  "Change the piece on the cell from previous-piece to new-piece."
  [previous-piece new-piece]
  new-piece)

(defn move-piece
  "Given the planned move of a given piece, and the previous move
  involving this piece, make sure that the board is updated accurately
  as the piece moves from the previous cell to the new cell."
  [[piece dest-cell] [[_ src-cell] _]]
  (commute (get-in board dest-cell) place piece)
  (commute (get-in board src-cell ) place :-)
  (commute num-moves inc))

(defn update-move-order-tuple
  "Update the stored `move-order-tuple` variable to the latest move."
  [move]
  (alter move-order-tuple
         #(vector (second %) move)))

(defn make-move
  "Pick the next move for the next player. Make the move."
  []
  (dosync
   (let [move (choose-move @move-order-tuple)]
     (move-piece move @move-order-tuple)
     (update-move-order-tuple move))))

(defn stress-ref [r]
  (let [slow-tries (atom 0)]
    (future
      (dosync
       (swap! slow-tries inc)
       (Thread/sleep 200)
       @r)
      (println (format "r is: %s, history: %d, after: %d tries"
                       @r (.getHistoryCount r) @slow-tries)))
    (dotimes [i 500]
      (Thread/sleep 10)
      (dosync (alter r inc)))
    :done))

(defn slow-conj
  [coll item]
  (Thread/sleep 10000)
  (conj coll item))


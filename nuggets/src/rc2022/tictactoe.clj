(ns rc2022.tictactoe
  "Write a program that lets two humans play a game of Tic Tac Toe in
  a terminal. The program should let the players take turns to input
  their moves. The program should report the outcome of the game.

  Later, add support for a computer player to your game. You can start
  with random moves and make the AI smarter if you have time."
  (:require
   [clojure.set :as cset]
   [clojure.test :as t]
   [clojure.test.check.clojure-test :as tcc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]))


;;; Instructions on how to play the game are at the very bottom of
;;; this file, in a rich comment box.

;; -------------------
;;; Setting up the game
;; -------------------
(declare win-sets)

(defn new-board
  "Returns a new board for the NxN `dimension`. This is mutable state,
  and is meant to be used throughout a single game."
  [dimension]
  (atom
   {:dimension dimension
    ;; These are the winning positions:
    :win-sets (win-sets dimension)
    ;; Player moves are tracked here:
    :moves {}
    ;; Anyone can start the game, post which this is tracked per move.
    :last-move-by nil}))

(defn new-player
  "Returns a new player of type `playing-as` for `board`. Board should
  be an atom as generated by `new-board`."
  [board playing-as]
  (swap! board assoc-in [:moves playing-as] #{}))

(defn new-game
  "Start a new game of tic-tac-toe on `board`. Setup the board and
  players. `board` should be an atom as generated by `new-board`.

  Starts a 2 player game with `x` and `o` symbols."
  [board]
  (new-player board :x)
  (new-player board :o)
  board)

(defn win-sets
  "For the given `dimension`, return all the winning positions (as
  sets for easy checking later)."
  [dimension]
  (let [ds (range dimension)
        rev-dims (reverse ds)]
    (concat
     ;; horizontal winning positions
     (map #(->> ds (interleave (repeat %)) (partition 2) (map vec) (into #{}))
          ds)
     ;; vertical winning positions
     (map #(->> (repeat %) (interleave ds) (partition 2) (map vec) (into #{}))
          ds)
     ;; the two diagonals
     (reduce (fn [[d1 d2] [i j]]
               [(conj d1 [i i]) (conj d2 [i j])])
             [#{} #{}]
             (partition 2 (interleave ds rev-dims))))))

;;; Playing the game!
;; -----------------

(defn make-move
  "Make the move! All checks have been done previously.
  Note: This is an internal function. You are probably looking for
  `play-move`."
  [board player-id move]
  (swap! board update-in [:moves player-id] conj move)
  (swap! board assoc :last-move-by player-id))

(t/deftest make-move-tests
  (let [b (new-game (new-board 3))]
    (make-move b :x [0 0])
    (t/is (= #{[0 0]} (get-in @b [:moves :x]))
          "A move is registered against :x")
    (t/is (= :x (:last-move-by @b))
          ":x made the last move")
    (make-move b :o [1 1])
    (t/is (= #{[1 1]} (get-in @b [:moves :o]))
          "A move is registered against :o")
    (t/is (= :o (:last-move-by @b))
          ":o made the last move")))

(defn can-i-play?
  "Return true if `player-id` can make the next move on `board`, false if
  we are still waiting for a move from someone else."
  [board player-id]
  (or (nil? (:last-move-by board))
      (not= player-id (:last-move-by board))))

(t/deftest can-i-play-tests
  (let [b (new-game (new-board 3))]
    (t/is (can-i-play? @b :x)
          "Anyone can start the game")
    (make-move b :x [0 0])
    (t/is (not (can-i-play? @b :x))
          ":x has just made a move, it is now :o's turn")
    (t/is (can-i-play? @b :o)
          "Yes, It's :o's turn")))

(defn occupied?
  "Return true if the given `move` on the `board` is already occupied."
  [board move]
  (contains? (apply cset/union (vals (:moves board))) move))

(t/deftest occupied-tests
  (let [b (new-game (new-board 3))]
    (t/is (false? (occupied? @b [0 0]))
          "New board is empty")
    (make-move b :x [0 0])
    (t/is (occupied? @b [0 0])
          ":x has played this move, it's occupied")
    (t/is (not (occupied? @b [2 2]))
          "This spot is still empty")
    (make-move b :o [2 2])
    (t/is (occupied? @b [2 2])
          "This spot is now occupied")))

(defn on-board?
  "Return true if the input `move` is valid for this `board`."
  [board move]
  (every? #(< -1 % (:dimension board)) move))

(tcc/defspec on-board-tests
  100
  (prop/for-all
   [dim (gen/fmap inc gen/nat)
    mx gen/nat
    my gen/nat]
   (let [board (new-board dim)]
     (cond
       (>= mx dim) (false? (on-board? @board [mx my]))
       (>= my dim) (false? (on-board? @board [mx my]))
       :else (true? (on-board? @board [mx my]))))))

(defn winning-position?
  "Return the player and the reason if this `board` is solved, else return nil."
  [board]
  (reduce-kv (fn [_a player mvs]
               (if-let [ws (some #(when (empty? (cset/difference % mvs)) %)
                                 (:win-sets board))]
                 (reduced [player ws])
                 nil))
             nil
             (:moves board)))

(tcc/defspec winning-position-tests
  100
  (prop/for-all
   ;; make sure 0, 1 is not generated for dim
   [[dim row col] (gen/let [dim (gen/fmap #(+ 2 %) gen/nat)]
                    [dim (rand-nth (range dim)) (rand-nth (range dim))])]
   (let [board (new-game (new-board dim))
         row-winner (map (fn [i] [row i]) (range dim))
         col-winner (map (fn [i] [i col]) (range dim))]
     (and (winning-position? (update-in @board [:moves :x] into row-winner))
          (winning-position? (update-in @board [:moves :o] into col-winner))
          ;; :o wins even if they've played more than the optimal
          ;; number of moves
          (winning-position? (update-in @board
                                        [:moves :o]
                                        into
                                        (conj col-winner [row col])))
          ;; :o does not win if they don't have a winning number of
          ;; cells filled
          (not (winning-position? (update-in @board
                                             [:moves :o]
                                             into
                                             (list [row col]))))))))

(defn all-moves
  "For a given `dimension` of board, generate all the possible cell values."
  [dimension]
  (mapcat (fn [row] (map (fn [col] [row col]) (range dimension)))
          (range dimension)))

(defn draw-position?
  "Return true if this `board` is a draw, with no victor."
  [board]
  (and (not (winning-position? board))
       (every? (partial occupied? board) (all-moves (:dimension board)))))

(defn complete?
  "Return true if the game is complete and cannot be continued."
  [board]
  (or (winning-position? board)
      (draw-position? board)))

(defn display-board
  "Render the `board` for humans"
  [board]
  (println "Current Board Status: \n")
  (doseq [i (range (:dimension board))]
    (doseq [j (range (:dimension board))]
      (when (reduce-kv (fn [_e? player-id moves]
                         (if (contains? moves [i j])
                           (do (print (format "| %s |" player-id))
                               (reduced false))
                           true))
                       true
                       (:moves board))
        (print "| __ |")))
    (println)
    (println (apply str (repeat (* 6 (:dimension board)) "-"))))
  (println "\nLast Move was played by: " (:last-move-by board))
  (when-let [[p moves] (winning-position? board)]
    (println (format "\nPlayer %s has a *winning position*!: %s"
                     p moves)))
  (when (draw-position? board)
    (println "\nThis board is drawn! Fight another battle!")))

(defn play-move
  "Play the `move` on the `board`, for `player-id`.

  `board` should be an atom as returned by `new-game`.
  `move` should be a [row, col] representation of a spot on the board."

  [board player-id move]
  (cond
    (complete? @board)
    (println "This board is complete! Fight a new battle!")

    (not (can-i-play? @board player-id))
    (println (format "%s can't play right now, waiting for the other player"
                     player-id))

    (not (on-board? @board move))
    (println (format "%s is not a valid position on board of dimensions %sX%s"
                     move
                     (:dimension @board)
                     (:dimension @board)))

    (occupied? @board move)
    (println (format "%s position is already occupied on the board."
                     move))

    :else
    ;; all checks passed, make the move!
    (-> board
        (make-move player-id move)
        display-board)))

;;; How to play the game:
;; ---------------------
;; Execute each line in this comment box on the REPL to see an example
;; game in progress.

(comment
  ;; Set up a new game. `b*` tracks the board.
  (let [b (new-game (new-board 3))]
    (def b* b))

  (display-board @b*)     ; Display the empty board
  (play-move b* :x [0 0]) ; Play this move on the board as X
  (play-move b* :o [2 2]) ; ... so on and so forth, until the game
                          ; completes.
  (play-move b* :x [2 0])
  (play-move b* :o [1 1])
  (play-move b* :x [1 0]))
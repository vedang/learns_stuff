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

;;; Initial thoughts:

;; Assumptions: The game will only be played between two players, and
;; not more than two players. P1 will be X and P2 will be O

;; The game board and current player should be refs that are updated
;; in tandem. Why? It will allow me to have two computer players play
;; against each other in the future.

;; start-game -> play-move -> check-move -> move -> wins? -> update
;; board. display-board displays the current state of the board, and
;; which player is going to play next.

(defn win-sets
  "For the given `dimension`, return the list of winning positions (as
sets for easy checking later)."
  [dimension]
  (let [dims (range dimension)
        rev-dims (reverse dims)]
    (concat
     ;; horizontal winning positions
     (map (fn [i]
            (->> dims
                (interleave (repeat i))
                (partition 2)
                (into #{})))
          dims)
     ;; vertical winning positions
     (map (fn [j]
            (->> (repeat j)
                (interleave dims)
                (partition 2)
                (into #{})))
          dims)
     ;; the two diagonals
     (reduce (fn [[d1 d2] [i j]]
               [(conj d1 (list i i))
                (conj d2 (list i j))])
             [#{} #{}]
             (partition 2 (interleave dims rev-dims))))))

(defn new-board
  "Returns a new board structure for the given `dimension`."
  [dimension]
  {:dimension dimension
   :win-sets (win-sets dimension)
   :x #{}
   :o #{}
   :last-move-by nil})

(defn new-player
  "Returns a new player of type `playing-as`"
  [playing-as]
  {:id playing-as
   :moves []})

(defn start-game
  "Start a new game of tic-tac-toe on a NxN `dimension` board. Setup the
board and player refs."
  [dimension]
  (let [board (ref (new-board dimension))
        x (ref (new-player :x))
        o (ref (new-player :o))]
   [board x o]))

(defn can-i-play?
  "Return true if `player` can make the next move on `board`, false if
we are still waiting for a move from someone else."
  [board player]
  (or (nil? (:last-move-by board))
      (not= (:id player) (:last-move-by board))))

(defn occupied?
  "Return true if the given `move` on the `board` is already occupied."
  [board move]
  (or (contains? (:x board) move)
      (contains? (:o board) move)))

(defn on-board?
  "Return true if the input move is valid for this board."
  [board move]
  (every? #(< -1 % (:dimension board)) move))

(defn make-move
  "Make the move! All checks have been done previously."
  [board player move]
  (dosync
   (alter board update (:id @player) conj move)
   (alter player update :moves conj move)
   (alter board assoc :last-move-by (:id @player))))

(defn winning-position?
  "Return the player and the reason if this board is solved, else return nil."
  [board]
  (let [x-wins-by (some (partial every? (:x board)) (:win-sets board))
        o-wins-by (some (partial every? (:o board)) (:win-sets board))]
    (cond
      x-wins-by [:x x-wins-by]
      o-wins-by [:o o-wins-by]
      :else nil)))

(defn display-board
  "Render the `board` for humans"
  [board]
  (println "Current Board Status: \n")
  (doseq [i (range (:dimension board))]
    (doseq [j (range (:dimension board))]
      (cond
        (contains? (:x board) [i j]) (print "| X |")
        (contains? (:o board) [i j]) (print "| O |")
        :else (print "| _ |")))
    (println)
    (println (apply str (repeat (* 5 (:dimension board)) "-"))))
  (println "\nLast Move was played by: " (:last-move-by board))
  (when-let [[p moves] (winning-position? board)]
    (println (format "\nPlayer %s has a *winning position*!: %s"
                     p moves))))

(defn play-move
  "Play the `move` on the `board`, for `player`.

  `player` and `board` should be refs returned by `start-game`. `move`
  should be a [row, col] representation of a spot on the board."

  [board player move]
  (cond
    (not (can-i-play? @board @player))
    (println (format "%s can't play right now, waiting for the other player"
                     (:id @player)))

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
    (dosync
     (make-move board player (seq move))
     (display-board @board))))


;;; Tests!
(t/deftest can-i-play-tests
  (let [[b p1 p2] (start-game 3)]
    (t/is (can-i-play? @b @p1)
          "Anyone can start the game")
    (make-move b p1 [0 0])
    (t/is (not (can-i-play? @b @p1))
          "p1 has just make a move, it is now p2s turn")
    (t/is (can-i-play? @b @p2)
          "It's p2s turn")))

(tcc/defspec on-board-tests
  100
  (prop/for-all
   [dim gen/nat
    mx gen/nat
    my gen/nat]
   (let [board (new-board dim)]
     (cond
       (>= mx dim) (false? (on-board? board [mx my]))
       (>= my dim) (false? (on-board? board [mx my]))
       :else (true? (on-board? board [mx my]))))))

(t/deftest occupied-tests
  (let [[b p1 p2] (start-game 3)]
    (t/is (false? (occupied? @b [0 0]))
          "New board is empty")
    (make-move b p1 [0 0])
    (t/is (occupied? @b [0 0])
          "p1 has played this move, it's occupied")
    (t/is (not (occupied? @b [2 2]))
          "This spot is still empty")
    (make-move b p2 [2 2])
    (t/is (occupied? @b [2 2])
          "This spot is now occupied")))

;;; How to play the game
(comment
  (let [[b x o] (start-game 3)]
    (def b* b)
    (def x* x)
    (def o* o))

  (display-board @b*)
  (play-move b* x* '(0 0))
  (play-move b* o* '(2 2))
  (play-move b* x* '(2 0))
  (play-move b* o* '(1 1))
  (play-move b* x* '(1 0)))

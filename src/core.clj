(ns TicTacToe.core

  (import java.util.Scanner)

  (import javax.swing.JFrame)

  (import java.awt.Font))



;Game Logic

(def x-set (atom #{}))

(def o-set (atom #{}))





(defn drawFrame []

  (def frame (JFrame. "Test"))

  (doto frame

    (.setSize 300 322) ;Account for top bar

    (.toFront)

    (.setVisible true)) frame

  (.getGraphics (.getContentPane frame)))



(def g (drawFrame)) ;This sets up the frame

(defn makeBlue [] (doto g (.setColor (java.awt.Color/BLUE)) 

                    (.drawLine 0 100 300 100)

                    (.drawLine 0 200 300 200)

                    (.drawLine 100 0 100 300)

                    (.drawLine 200 0 200 300)))

(def font (new Font "Courier New", 1, 90))

(doto g (.setFont font))



(defn draw_x [position] (doto g 

                          (.drawString "X" 

                            (+ (* 100 (position 0)) 20) 

                            (+ (* 100 (position 1)) 70)))) ; xpos * 100 + 20 , ypos * 100 + 70

(defn draw_o [position] (doto g 

                          (.drawString "O" 

                            (+ (* 100 (position 0)) 20) 

                            (+ (* 100 (position 1)) 70)))) ; xpos * 100 + 20 , ypos * 100 + 70



(defn clear [] "Clears the board" (doto g (.setColor (java.awt.Color/WHITE)) (.fillRect 0 0 300 300)) (makeBlue) (def x-set (atom #{})) (def o-set (atom #{})))



(defn set-piece "Sets a position if not taken" [x-or-o coord]  

  (if x-or-o 

    (if-not (@o-set coord) (swap! x-set conj coord)) 

    (if-not (@x-set coord) (swap! o-set conj coord))))



(defn remove-piece [x-or-o coord] (if x-or-o 

                                    (swap! x-set (fn [arg-coord] (set (remove #(= coord %) arg-coord))))

                                    (swap! o-set (fn [arg-coord] (set (remove #(= coord %) arg-coord))))))

(defn add-coords "v1 + v2" [v1 v2] [(+ (v1 0) (v2 0)) 

                                    (+ (v1 1) (v2 1))])

(defn sub-coords "v1 - v2" [v1 v2] [(- (v1 0) (v2 0)) 

                                    (- (v1 1) (v2 1))])

(defn delta-coords "v2 - v1" [v1 v2] [(- (v2 0) (v1 0)) 

                                      (- (v2 1) (v1 1))])

(defn get-neighbors "Returns the neighbouring Xs of a coordinate if true, neighbouring Os if false" [x-or-o coord] 

  (filter identity (map (if x-or-o @x-set @o-set) ;includes coord in the list

                        (map (partial add-coords coord) 

                             (for [x [-1 0 1] 

                                   y [-1 0 1] 

                                   :when (not= x y 0)] 

                               [x y])))))

(defn get-value-coord "Returns the string value of a coord, either X, O or -" [coord] (if (@x-set coord) "X" (if (@o-set coord) "O" "-")))

(defn is-valid-coord "Returns if the coordinate is a valid coordinate on the board (boolean)" 

  [[x y]] 

  (and (number? x) (number? y) (and (> 3 x) (< -1 x))(and (> 3 y) (< -1 y))))

(defn is-valid-move "Returns true if the spot on the board is not taken, else false" [coord] (and (is-valid-coord coord) (not (or (@x-set coord) (@o-set coord)))))

(defn is-board-full "Returns true if board is full, else false" []

  (empty? (filter identity (map is-valid-move 

                                (for [x [0 1 2] y [0 1 2]] [x y])))))

;WIN CONDITIONS

;Middle=(map (partial sub-coords [1 1]) (map (partial delta-coords [1 1]) (get-neighbors true [1 1]))) ;Reflection of list over point (aka check if in middle returns two trues)

;Side=(map add-coords (map (partial delta-coords [1 1]) (get-neighbors true [1 1])) (get-neighbors true [1 1])) ;Reflection point over list (aka check if on side returns one true)

(defn reflected-moves "Returns a list of all the moves, both available and unavailable, of a coordinate reflected from and to all of its neighbouring pieces on the board" [x-or-o coord]

  (filter is-valid-coord (concat 

                           (map (partial sub-coords coord) (map (partial delta-coords coord) (get-neighbors x-or-o coord))) 

                           (map add-coords (map (partial delta-coords coord) (get-neighbors x-or-o coord)) (get-neighbors x-or-o coord)))))

(defn win? "Returns if a piece placed wins the game (boolean)" [x-or-o coord] (not (empty? (filter identity (map (if x-or-o @x-set @o-set) (reflected-moves x-or-o coord))))))

(defn get-coord "Gets the coord from console if there is one" [] 

  (let [str (read-line) sc (Scanner. str)] 

    (if (.hasNextInt sc)

      (let [x (.nextInt sc)] 

        (if (.hasNextInt sc) 

          (let [y (.nextInt sc)] (if (is-valid-move [x y]) [x y] nil)))))))

(defn get-valid-move "Uses helper method get-coord to loop until a valid untaken spot is found" []

  (loop [coord (get-coord)] (if coord coord (do (println "Invalid coordinate") (recur (get-coord))))))

(defn board-to-string "Returns a string of the current board" [] 

  (apply str (map #(if (is-valid-coord %) (get-value-coord %) (identity %)) 

                  (apply concat (for [y [2 1 0]] (conj (for [x [0 1 2]] [x y]) "\r\n"))))))

(defn driver-human-human "Drives a human vs human game" [] (clear) (loop [move (get-valid-move) x-or-o true] 

                                                                     (set-piece x-or-o move) (println (board-to-string)) (doall (map draw_x @x-set)) (doall (map draw_o @o-set)) 

                                                                     (cond

                                                                       (win? x-or-o move) (println (if x-or-o "x" "o") "wins")

                                                                       (is-board-full) (println "Draw")

                                                                       :else (recur (get-valid-move) (not x-or-o)))))



;AI

(defn all-pos "Returns a list of all positions" [] (for [x [0 1 2] 

                                                         y [0 1 2]] 

                                                     [x y]))

(defn possible-pos "Returns a list of untaken positions" [] (remove 

                                                              #(or (contains? @x-set %) 

                                                                   (contains? @o-set %)) 

                                                              (all-pos)))

(defn win-pos "Returns a list of winning positions" [x-or-o] (filter #(win? x-or-o %) (possible-pos)))

(defn diff-win-pos "Returns the difference in win positions a move would create" [x-or-o coord] (if (not (is-valid-move coord)) -1 (let [before (count (win-pos x-or-o))

                                                                                                                                         after (do (set-piece x-or-o coord) (count (win-pos x-or-o)))]

                                                                                                                                     (remove-piece x-or-o coord) (- after before))))

(defn fork? "Returns true if a move would create a fork, else false" [x-or-o coord] (= 2 (diff-win-pos x-or-o coord)))

(defn place-two? "Returns true if a move would both create two in a row, and the opponent's forced move is not a fork, else false" [x-or-o coord] 

  (and (= 1 (diff-win-pos x-or-o coord))) (if (empty? (reflected-moves x-or-o coord)) false (not (fork? (not x-or-o) (first (filter is-valid-move (reflected-moves x-or-o coord)))))))



(defn forks "Returns a list of positions that would create a fork" [x-or-o] (filter #(fork? x-or-o %) (possible-pos)))

(defn center "Returns the center if it is available" [] (if (is-valid-move [1 1]) '([1 1])))

(defn corners "Returns the corners that are available, if any" [] (filter #(is-valid-move %) '([0 0] [0 2] [2 0] [2 2])))

(defn sides "Returns the sides that are available, if any" [] (filter #(is-valid-move %) '([0 1] [1 0] [2 1] [1 2])))

(defn next-move "Returns the next move to be done" [x-or-o] (if (empty? @x-set) [0 0] (first (concat (win-pos x-or-o) (win-pos (not x-or-o)) (forks x-or-o) 

                                                                                                     (filter #(place-two? x-or-o %) (possible-pos)) (forks (not x-or-o)) 

                                                                                                     (center) (corners) (sides)))))

(defn do-next-move "Does the next move to be done" [x-or-o] (set-piece x-or-o (next-move x-or-o)))

(defn driver-ai-ai "Drives an ai vs ai game" [] (clear) (loop [move (next-move true) x-or-o true] 

                                                          (set-piece x-or-o move) (println (board-to-string)) (doall (map draw_x @x-set)) (doall (map draw_o @o-set)) (Thread/sleep 500)

                                                          (cond

                                                            (win? x-or-o move) (println (if x-or-o "x" "o") "wins")

                                                            (is-board-full) (println "Draw")

                                                            :else (recur (next-move (not x-or-o)) (not x-or-o)))))

(defn driver-human-ai "Drives a human vs ai game, where the human is x" [human-goes-first] (clear) (loop [move (if human-goes-first (get-valid-move) (next-move true)) x-or-o true]

                                                                                                     (set-piece x-or-o move) (println (board-to-string)) (doall (map draw_x @x-set)) (doall (map draw_o @o-set)) (Thread/sleep 500)

                                                                                                     (cond

                                                                                                       (win? x-or-o move) (println (if x-or-o "x" "o") "wins")

                                                                                                       (is-board-full) (println "Draw")

                                                                                                       :else (recur 

                                                                                                               (if (not= x-or-o human-goes-first) (get-valid-move) (next-move (not x-or-o)))

                                                                                                               (not x-or-o)))))







(driver-ai-ai)
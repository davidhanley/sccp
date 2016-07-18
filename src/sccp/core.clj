(ns sccp.core
  (:gen-class))

(use '[clojure.set :only (map-invert)])

(defn l [] (use 'sccp.core :reload))

; the indices for all the chess squares
(def square-indices (range 0 64))

;the coordinates for all the chess squares 
(def square-coordinates (map #(hash-map :r (quot % 8) :f (mod % 8)) square-indices))

;make the symbols for the chess squares
(def files (vec "abcdefgh"))
(def ranks (vec "87654321"))
(def square-strings (map #(str (files (:f %))(ranks (:r %))) square-coordinates)) 

;and now the symbols 
(def square-symbols (map keyword square-strings))

; mapping from coordinates to squares
(def coord-to-square (zipmap square-coordinates square-symbols))
(def square-to-coord (map-invert coord-to-square))

(defn to-coord[sq](or (square-to-coord sq) sq))
(def to-coords (partial map (fn[[r f]]{:r r :f f})))

(def add-coords (partial merge-with +))

(def knight-deltas [[-2 -1][-2 1][-1 -2][-1 2][1 -2][1 2][2 -1][2 1]])
(def king-deltas [[-1 -1][-1 0][-1 1][0 -1][0 1][1 -1][1 0][1 1]])

(def flip-side {:white :black :black :white})

(defn play-move[bd move]
     (let [f (:f move)
           t (:t move)
           moving (:to-move bd)
           enemy (flip-side moving)
           mine (moving bd)
           his (enemy bd)
           piece (f (moving bd))]
       (assoc bd
         :to-move (flip-side moving)
         moving (dissoc (assoc mine t piece) f)
         enemy (dissoc his t))))

(defn coordify[cos](or (coord-to-square cos) cos))

(defn make-move[f t & rest](let [cf (coordify f) ct (coordify t)] (apply assoc {} :f cf :t ct rest)))
 

(defn hopper-gen[delt] 
  (into {} (map (fn [[c sq]][sq (map (partial make-move c) (filter coord-to-square (map (partial add-coords c) (to-coords delt))))]) coord-to-square)))

(def knight-moves (hopper-gen knight-deltas))

(def king-moves (hopper-gen king-deltas))

(defn generate-moves-for-hopper[lookup board from-square]
  (let [moving (board :to-move)
        my-guys (board moving)
        pseudomoves (lookup from-square)]
    (filter (fn[mv](not (my-guys (:t mv)))) pseudomoves)))
           
(defn ray[s d](map (partial make-move s) (rest (take-while coord-to-square (iterate (partial add-coords d) s)))))

(def rook-deltas [[1 0][-1 0][0 1][0 -1]])
(def bishop-deltas [[-1 -1][-1 1][1 -1][1 1]])

(defn moves-for-slider[deltas square](filter not-empty (map (partial ray square) (to-coords deltas))))

(defn slider-gen[deltas] (into {} (map (fn[[c sq]][sq (moves-for-slider deltas c)]) coord-to-square)))

(def rook-moves (slider-gen rook-deltas))
(def bishop-moves (slider-gen bishop-deltas))
(def queen-moves (merge-with concat rook-moves bishop-moves))

(declare white-pawn-gen black-pawn-gen)

(defn side-for-value[value](if (< value 0) :black :white))

(defn defpiece[n g v gen ev](list 'def n {:glyph (first g) :value v :side (side-for-value v) :generator gen :eval ev}))
(defmacro defpieces[& args](let [el (partition 5 args)
                                 names (vec (map first el))](concat ['do] (map #(apply defpiece %) el)(list (list 'def 'pieces names)))))

(defpieces wpawn   "P" 100 nil nil 
           wknight "N" 325 (partial generate-moves-for-hopper knight-moves) nil 
           wbishop "B" 350 nil nil
           wrook   "R" 500 nil nil
           wqueen  "Q" 950 nil nil
           wking   "K" 10000 (partial generate-moves-for-hopper king-moves) nil 
           bpawn   "p" -100 nil nil 
           bknight "n" -325 (partial generate-moves-for-hopper knight-moves) nil 
           bbishop "b" -350 nil nil
           brook   "r" -500 nil nil
           bqueen  "q" -950 nil nil
           bking   "k" -10000 (partial generate-moves-for-hopper king-moves) nil)

(def char-to-piece (zipmap (map :glyph pieces) pieces))

(defn promotify[m]
  (let [r (:r (to-coord (:t m)))]
    (cond (= r 0)(map (partial assoc m :promotes) [wqueen wrook wbishop wknight])
          (= r 7)(map (partial assoc m :promotes) [bqueen brook bbishop bknight])
          true [m])))

(defn pawn-move[delta c] 
     (let [e #(add-coords c {:r delta :f %})] 
     {:forward (promotify (make-move c (e 0)))
      :captures (apply concat (map promotify (map (partial make-move c) (filter coord-to-square (map e [-1 1])))))}))

(defn pawngen[d](into {} (map (fn [[c sq]][sq (pawn-move d c)]) coord-to-square)))

(def white-pawn-moves (pawngen -1))
(def black-pawn-moves (pawngen 1))

(def start-string 
  (concat "rnbqkbnr" 
          "pppppppp" 
          "        "
          "        "
          "        "
          "        "
          "PPPPPPPP"
          "RNBQKBNR"))    

(defn string-to-board[s]
  (let [pieces (map char-to-piece s)
        piece-squares (zipmap square-symbols pieces) 
        side #(into {} (filter (fn [[s p]](= (:side p) %)) piece-squares)) ]
	{:to-move :white
         :white (side :white)
         :black (side :black)}))

(def sb (string-to-board start-string))

; no need for zorbist codes, the board itself is a hash(!)
(def board-hash (atom {}))

(defn side-material[board side](reduce + (map :value (vals (side board)))))

(defn sum-material[board](+ (side-material board :white)(side-material board :black)))

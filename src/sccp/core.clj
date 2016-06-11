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

(def add-coords (partial merge-with +))

(def to-coords (partial map (fn[[r f]]{:r r :f f})))

(def knight-deltas [[-2 -1][-2 1][-1 -2][-1 2][1 -2][1 2][2 -1][2 1]])
(def king-deltas [[-1 -1][-1 0][-1 1][0 -1][0 1][1 -1][1 0][1 1]])

(defn make-move[f t]{:f (coord-to-square f) :t (coord-to-square t)})

(defn hopper-gen[delt] 
  (into {} (map (fn [[c sq]][sq (map (partial make-move c) (filter coord-to-square (map (partial add-coords c) (to-coords delt))))]) coord-to-square)))

(def knight-moves (hopper-gen knight-deltas))

(def king-moves (hopper-gen king-deltas))

(defn ray[s d](map (partial make-move s) (rest (take-while coord-to-square (iterate (partial add-coords d) s)))))

(def rook-deltas [[1 0][-1 0][0 1][0 -1]])
(def bishop-deltas [[-1 -1][-1 1][1 -1][1 1]])

(defn moves-for-slider[deltas square](filter not-empty (map (partial ray square) (to-coords deltas))))

(defn slider-gen[deltas] (into {} (map (fn[[c sq]][sq (moves-for-slider deltas c)]) coord-to-square)))

(def rook-moves (slider-gen rook-deltas))
(def bishop-moves (slider-gen bishop-deltas))
(def queen-moves (merge-with concat rook-moves bishop-moves))

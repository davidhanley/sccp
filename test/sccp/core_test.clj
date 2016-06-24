(ns sccp.core-test
  (:require [clojure.test :refer :all]
            [sccp.core :refer :all]))

(deftest square-test
  (testing "basic chess squares stats"
    (is (= (count square-indices) 64)))
  (testing "coords to squares"
    (is (= (coord-to-square {:r 1 :f 1}) :b7)) 
    (is (= (coord-to-square {:r 9 :f 1}) nil)
    (is (= (coord-to-square {:r 4 :f 6}) :g4))))
  (testing "square math"
    (is (= (add-coords {:r 3 :f 4} {:r 1 :f 1}) {:r 4 :f 5}))
    (is (= (add-coords {:r 3 :f 4} {:r 0 :f -1}) {:r 3 :f 3})))  	     
           
)

(deftest basic-movegen
  (testing "ray casting"
    (is (= (count (moves-for-slider rook-deltas {:r 0 :f 0})) 2))
    (is (= (count (moves-for-slider bishop-deltas {:r 0 :f 0})) 1)))
)

(deftest piece-lookup
  (testing "piece count right"
    (is (= (count pieces) 12)))
  (testing "lookups look good"
    (is (every? #(< (:side (char-to-piece %) 0)) "pnbrqk"))
    (is (= (char-to-piece \N) wknight))
  )
 
)

(deftest move-lookups 
  (testing "knight move counts"
    (is (= (count (knight-moves :e4)) 8))
    (is (= (count (knight-moves :h1)) 2)))
  (testing "king-move-counts"
    (is (= (count (king-moves :e4)) 8))
    (is (= (count (king-moves :e1)) 5))
    (is (= (count (king-moves :h1)) 3)))
  (testing "bishop-move-counts"
    (is (= (count (reduce concat (bishop-moves :h1))) 7))
    (is (= (count (reduce concat (bishop-moves :e4))) 13)))
  (testing "pawn move counts"
    (is (= (count (:forward (white-pawn-moves :e4))) 1))
    (is (= (count (:captures (white-pawn-moves :e4))) 2))
    (is (= (count (:captures (white-pawn-moves :h4))) 1))
    (is (= (count (:captures (white-pawn-moves :h7))) 4))
    (is (= (count (:captures (white-pawn-moves :e7))) 8))
)

)

(deftest play-testing 
  (testing "simple moves work"
    (let [sm (make-move :e2 :e4)
          new-board ((:player sm) sb)]
      (is (= (:f sm) :e2))
      (is (= (:t sm) :e4))
      (is (= (:e2 (:white new-board)) nil))
      (is (= (:e4 (:white new-board)) wpawn))
      )
    )
)


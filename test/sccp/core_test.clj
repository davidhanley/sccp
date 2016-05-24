(ns sccp.core-test
  (:require [clojure.test :refer :all]
            [sccp.core :refer :all]))

(deftest square-test
  (testing "basic chess squares stats"
    (is (= (count squares) 64)))
  (testing "square to coords"
    (is (= (count squares-to-coords) 64))
    (is (= (squares-to-coords 4) {:r 0, :f 4}))
    (is (= (squares-to-coords 444) nil)))
  (testing "coords to squares"
    (is (= (coords-to-squares {:r 1 :f 1}) 9))
    (is (= (coords-to-squares {:r 1 :f 1}) 9)) 
    (is (= (coords-to-squares {:r 9 :f 1}) nil)
    (is (= (coords-to-squares {:r 4 :f 6}) 38))))
)



(ns sccp.core
  (:gen-class))
(use '[clojure.set :only (map-invert)])

(defn l [] (use 'sccp.core :reload))

(def squares (range 0 64))

(def squares-to-coords (zipmap squares (map #(hash-map :r (quot % 8) :f (mod % 8)) squares)))
(def coords-to-squares (map-invert squares-to-coords))

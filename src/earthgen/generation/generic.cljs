(ns earthgen.generation.generic
  (:require [earthgen.math.random :as random]
            [earthgen.generation.core :as generation]
            [earthgen.generation.terrain :as terrain]))

(defn heightmap [args]
  ["heightmap" args])

(defn terrain [seed sea-level method]
  [["with-seed" (if seed {:seed seed} {})]
   method
   ["with-sea-level" {:sea-level sea-level}]])

(def transforms
  {"with-seed" (fn [[args]] (generation/with-seed (:seed args)))
   "heightmap" (fn [[args]] (apply terrain/heightmap (map args [:granularity :irregularity :amplitude :seed])))
   "with-sea-level" (fn [[args]] (terrain/sea-level (:sea-level args)))})

(defn input-transforms [ls]
  (mapv (fn [a]
          ((get transforms (first a)) (rest a)))
        ls))

(defn from-input [ls]
  (let
   [seed (random/random-seed 12)]
    (cons (let
           [[k args] (first ls)]
            (if (= "with-seed" k)
              [k (update args :seed (fn [a] (or a seed)))]
              [k args]))
          (rest ls))))

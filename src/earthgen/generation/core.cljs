(ns earthgen.generation.core
  (:require [earthgen.math.random :as random]))

(defn with-seed [seed]
  (fn [_]
    (fn [[rng result]]
      [(if seed (random/with-seed seed) rng) result])))

(defn transform [grids subdivisions ls]
  (let
   [grids (take (inc subdivisions) grids)]
    (reduce (fn [result f]
              ((f grids) result))
            [(random/with-seed (random/random-seed 256)) (last grids)]
            ls)))

(ns earthgen.generation.core
  (:require [earthgen.math.random :as random]
            [earthgen.grid.core :as grid]
            [earthgen.math.vector :as vector]))

(defn timed-subdivision [[_ grid]]
  (let
   [start (.now js/Date)
    subdivided (grid/subdivide grid)]
    [(- (.now js/Date) start) subdivided]))

(defn grids-with-timeout [grids timeout subdivisions]
  (if (not timeout)
    [subdivisions (mapv second (take (inc subdivisions) grids))]
    (loop [time 0
           n 0]
      (let
       [taken (doall (take (inc n) grids))
        [t _] (last taken)
        total (+ time t)]
        (if (or (= n subdivisions)
                (< timeout total))
          [n (mapv second taken)]
          (recur total (inc n)))))))

(defn with-seed [seed]
  (fn [_]
    (fn [[rng result]]
      [(if seed (random/with-seed seed) rng) result])))

(def default-rotation [1 0 0 0])

(defn with-rotation [rotation]
  (let
   [normal (vector/normal rotation)
    valid? (and (not (vector/invalid? normal))
                (= 4 (count normal)))]
    (fn [_]
      (fn [[rng result]]
        (and valid? [rng (assoc result :rotation normal)])))))

(defn transform [grids ls]
  (reduce (fn [result f]
            ((f grids) result))
          [(random/with-seed (random/random-seed 256)) (last grids)]
          (cons (with-rotation default-rotation)
                ls)))

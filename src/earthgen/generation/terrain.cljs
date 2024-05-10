(ns earthgen.generation.terrain
  (:require [earthgen.interop.array :as js-array]
            [earthgen.math.random :as random]))

(def tile-count (comp js-array/count :tile-vertices))
(def corner-count (comp js-array/count :corner-vertices))

(defn to-elevation [amplitude]
  (fn [a] (* amplitude (- a 0.5))))

(defn with-tile-elevation [planet elevation]
  (assoc planet :tile-elevation elevation))

(defn with-corner-elevation [planet elevation]
  (assoc planet :corner-elevation (elevation planet)))

(defn midpoint-elevation [planet elevation]
  (let
   [tile-elevation (:tile-elevation planet)]
    (js-array/map (fn [corner-tiles elevation]
                    (let
                     [[a b c] corner-tiles]
                      (+ elevation
                         (* (/ 1 3)
                            (+ (js-array/get tile-elevation a)
                               (js-array/get tile-elevation b)
                               (js-array/get tile-elevation c))))))
                  (:corner-tiles planet)
                  elevation)))

(defn corner-elevation [midpoint? elevation]
  (fn [planet]
    (if midpoint?
      (midpoint-elevation planet elevation)
      elevation)))

(defn next-scale [irregularity scale]
  (* scale (Math/pow 2 (- irregularity 1))))

(defn heightmap [granularity irregularity amplitude seed]
  (fn [grids]
    (fn [[outer-rng planet]]
      (let
       [grid-count (count grids)
        grids (drop (min granularity (dec (count grids)))
                    grids)
        grid (first grids)
        midpoint? (<= granularity grid-count)
        scale (if midpoint? (next-scale irregularity 1) 1)
        [outer-rng new-seed] (random/pseudo-random-seed 256 outer-rng)
        seed (if seed seed new-seed)
        rng (random/with-seed seed)
        [rng tiles] (random/take (tile-count grid) rng)
        [rng corners] (random/take (corner-count grid) rng)
        initial (-> grid
                    (with-tile-elevation (js-array/map (to-elevation amplitude) tiles))
                    (with-corner-elevation (corner-elevation midpoint? (js-array/map (to-elevation (* scale amplitude)) corners))))]
        (loop [rng rng
               scale scale
               grids (rest grids)
               result initial]
          (if (empty? grids)
            [outer-rng (merge planet (select-keys result [:tile-elevation :corner-elevation]))]
            (let
             [grid (first grids)
              scale (next-scale irregularity scale)
              [rng nums] (random/take (corner-count grid) rng)
              result (-> grid
                         (with-tile-elevation (js-array/concat (:tile-elevation result) (:corner-elevation result)))
                         (with-corner-elevation (corner-elevation true (js-array/map (to-elevation (* scale amplitude)) nums))))]
              (recur rng scale (rest grids) result))))))))

(defn sea-level [level]
  (fn [_]
    (fn [[rng planet]]
      [rng (assoc planet :sea-level level)])))

(ns earthgen.db
  (:require [earthgen.graphics.models :as models]
            [earthgen.graphics.map-modes :as map-modes]
            [earthgen.grid.core :as grid]
            [earthgen.math.random :as random]
            [earthgen.generation.core :as generation]
            [earthgen.generation.terrain :as terrain]))

(def default-db
  (let
   [grids (iterate grid/subdivide (grid/initial))
    subdivisions 5
    seed (random/random-seed 12)
    granularity 2
    irregularity 0.4
    amplitude 8000
    sea-level 3000
    [_ planet]
    (generation/transform
     grids
     subdivisions
     [(generation/with-seed seed)
      (terrain/heightmap granularity irregularity amplitude seed)
      (terrain/sea-level sea-level)])
    perspective {:rotation {:latitude 0
                            :longitude 0}
                 :distance 3}]
    {:grids grids
     :planet planet
     :graphics {:shader nil
                :models [(models/solid-tiles
                          map-modes/elevation
                          planet)]
                :perspective perspective}
     :perspective perspective
     :time-per-frame 20
     :mouse-down false
     :mouse-event [:none [0 0]]}))

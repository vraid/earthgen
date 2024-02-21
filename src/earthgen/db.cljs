(ns earthgen.db
  (:require [earthgen.math.random :as random]
            [earthgen.math.projection :as projection]
            [earthgen.perspective :as perspective]
            [earthgen.graphics.models :as models]
            [earthgen.graphics.camera :as camera]
            [earthgen.graphics.map-modes :as map-modes]
            [earthgen.grid.core :as grid]
            [earthgen.generation.core :as generation]
            [earthgen.generation.generic :as generic]
            [earthgen.generation.predefined :as predefined]
            [earthgen.validation :as validation]))

(def default-db
  (let
   [grids (iterate grid/subdivide (grid/initial))
    subdivisions 7
    seed (random/random-seed 12)
    granularity 2
    irregularity 0.4
    amplitude 8000
    sea-level 3000
    simple-terrain {:seed seed
                    :granularity granularity
                    :irregularity irregularity
                    :amplitude amplitude
                    :sea-level sea-level}
    model (predefined/continents)
    [_ planet] (generation/transform
                grids
                subdivisions
                (generic/input-transforms model))
    perspectives
    {:spherical
     {:name "Spherical"
      :camera camera/spherical
      :update perspective/update-spherical
      :projection projection/identity
      :rotation {:latitude 0
                 :longitude 0}
      :distance 3}
     :hammer
     {:name "Hammer"
      :camera camera/hammer
      :update perspective/update-hammer
      :projection projection/hammer
      :scale 3}}
    current-perspective :spherical]
    {:grids grids
     :planet planet
     :graphics {:shader nil
                :models [(models/solid-tiles
                          (get-in perspectives [current-perspective :projection])
                          map-modes/elevation
                          planet)]
                :perspective (get perspectives current-perspective)}
     :perspectives perspectives
     :model model
     :view {:subdivisions (str subdivisions)
            :mode :predefined
            :simple-terrain (validation/simple-terrain-str-values simple-terrain)
            :current-perspective current-perspective}
     :time-per-frame 20
     :input {:mouse-down false
             :mouse-event [:none [0 0]]
             :touch-start false
             :touch-event [:none [] []]}}))

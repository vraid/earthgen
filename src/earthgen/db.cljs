(ns earthgen.db
  (:require [earthgen.math.random :as random]
            [earthgen.math.quaternion :as quaternion]
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
   [grids (iterate generation/timed-subdivision [0 (grid/initial)])
    startup-timeout 200
    loaded-timeout 2000
    [subdivisions used-grids] (generation/grids-with-timeout grids startup-timeout 20)
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
    model (generic/from-input (predefined/continents))
    [_ planet]
    (generation/transform
     used-grids
     (generic/input-transforms model))
    perspectives
    [[:spherical
      {:label "Spherical"
       :camera camera/spherical
       :init perspective/init-spherical
       :update perspective/update-spherical
       :projection projection/identity
       :rotation {:latitude 0
                  :longitude 0}
       :distance 3}]
     [:hammer
      {:label "Hammer"
       :camera camera/hammer
       :init perspective/init-hammer
       :update perspective/update-hammer
       :projection projection/hammer
       :scale 3}]]
    perspective-dict (into {} perspectives)
    current-perspective :spherical]
    {:grids grids
     :planet planet
     :graphics {:shader nil
                :buffers nil
                :models [(models/contoured-tiles
                          (get-in perspective-dict [current-perspective :projection])
                          map-modes/elevation
                          planet)]
                :perspective (get perspective-dict current-perspective)}
     :perspectives perspective-dict
     :model model
     :view {:subdivisions (str subdivisions)
            :subdivision-timeout (str loaded-timeout)
            :mode :predefined
            :simple-terrain (validation/simple-terrain-str-values simple-terrain)
            :custom ""
            :perspectives (map (fn [[k v]] [k (:label v)]) perspectives)
            :current-perspective current-perspective
            :planet-rotation (:rotation planet)
            :current-rotation quaternion/identity}
     :time-per-frame 20
     :input {:mouse-down false
             :mouse-event [:none [0 0]]
             :touch-start false
             :touch-event [:none [] []]}}))

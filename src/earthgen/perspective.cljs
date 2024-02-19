(ns earthgen.perspective
  (:require [earthgen.math.vector :as vector]))

(defn current [db]
  (get-in db [:perspectives (get-in db [:view :current-perspective])]))

(defn clamp [a b c]
  (max a (min b c)))

(defn update-spherical [model bounding-rect initial-coord coord]
  (let
   [{:keys [latitude longitude]} (:rotation model)
    [_ _ width height] bounding-rect
    [xdiff ydiff] (vector/subtract coord initial-coord)
    updated
    {:latitude (clamp (* -0.5 Math/PI)
                      (* 0.5 Math/PI)
                      (+ latitude
                         (* ydiff 0.8 (/ Math/PI height))))
     :longitude (+ longitude
                   (* xdiff 0.8 (/ Math/PI width)))}]
    (fn [db]
      (assoc-in db [:perspectives :spherical :rotation] updated))))

(defn update-hammer [_ _ _ _]
  (fn [db]
    db))

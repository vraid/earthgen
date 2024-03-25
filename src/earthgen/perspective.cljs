(ns earthgen.perspective
  (:require [earthgen.math.vector :as vector]
            [earthgen.math.quaternion :as quaternion]
            [earthgen.math.projection :as projection]))

(defn current [db]
  (get-in db [:perspectives (get-in db [:view :current-perspective])]))

(defn clamp [a b c]
  (max a (min b c)))

(defn init-spherical [db model]
  (let
   [{:keys [latitude longitude]} (:rotation model)]
    (assoc-in db [:view :current-rotation] (projection/latitude-longitude-rotation latitude longitude))))

(defn update-spherical [model bounding-rect initial-coord coord]
  (let
   [{:keys [latitude longitude]} (:rotation model)
    [_ _ width height] bounding-rect
    [xdiff ydiff] (vector/subtract coord initial-coord)
    latitude (clamp (* -0.5 Math/PI)
                    (* 0.5 Math/PI)
                    (+ latitude
                       (* ydiff 0.8 (/ Math/PI height))))
    longitude (+ longitude
                 (* xdiff 0.8 (/ Math/PI width)))
    updated {:latitude latitude
             :longitude longitude}]
    (fn [db]
      (-> db
          (assoc-in [:perspectives :spherical :rotation] updated)
          (assoc-in [:view :current-rotation] (projection/latitude-longitude-rotation latitude longitude))))))

(defn init-hammer [db _]
  (assoc-in db [:view :current-rotation] quaternion/identity))

(defn update-hammer [_ _ _ _]
  (fn [db]
    db))

(ns earthgen.graphics.camera
  (:require [thi.ng.geom.vector :as v]
            [thi.ng.geom.matrix :as mat]
            [earthgen.math.quaternion :as quaternion]))

(defn spherical-matrix [a]
  (let
   [quat (quaternion/product-normal
          (quaternion/from-axis-angle [0 0 1] (get-in a [:rotation :longitude]))
          (quaternion/from-axis-angle [0 1 0] (get-in a [:rotation :latitude])))
    [r1 r2 r3] (quaternion/to-matrix quat)]
    (apply mat/->Matrix44
           (concat r1 [0] r2 [0] r3 [0] [0 0 0 1]))))

(defn spherical [a viewport]
  {:proj (mat/perspective 45 viewport -2 2)
   :view (mat/look-at (v/vec3 (:distance a) 0 0) (v/vec3 0 0 0) (v/vec3 0 0 1))
   :model (spherical-matrix a)})

(defn ortho [scale]
  (let
   [h scale
    w h]
    (mat/ortho (- w) h w (- h) -1 1)))

(defn hammer [a _]
  {:proj (ortho (:scale a))
   :model mat/M44})

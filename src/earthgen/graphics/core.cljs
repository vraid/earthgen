(ns earthgen.graphics.core
  (:require [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.shaders :as sh]
            [thi.ng.geom.gl.shaders.basic :as sh-basic]
            [thi.ng.geom.gl.webgl.constants :as glc]
            [thi.ng.geom.vector :as v]
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

(def default-shader (sh-basic/make-shader-spec-3d true))

(defn make-shader [gl]
  (sh/make-shader-from-spec gl default-shader))

(defn draw-canvas [gl shader uniforms models]
  (gl/set-viewport gl (gl/get-viewport-rect gl))
  (gl/cull-faces gl glc/back)
  (gl/clear-color-and-depth-buffer gl 0 0 0 1 1)
  (doseq [model models]
    (gl/draw-with-shader gl (-> model
                                (gl/make-buffers-in-spec gl glc/static-draw)
                                (update :uniforms merge uniforms)
                                (assoc :shader shader)))))

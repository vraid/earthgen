(ns earthgen.graphics.core
  (:require [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.shaders :as sh]
            [thi.ng.geom.gl.shaders.basic :as sh-basic]
            [thi.ng.geom.gl.webgl.constants :as glc]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.matrix :as mat]))

(def default-shader (sh-basic/make-shader-spec-3d true))

(defn make-shader [gl]
  (sh/make-shader-from-spec gl default-shader))

(defn spherical [viewport]
  {:proj (mat/perspective 45 viewport -2 2)
   :view (mat/look-at (v/vec3 3 0 0) (v/vec3 0 0 0) (v/vec3 0 0 1))
   :model mat/M44})

(defn draw-canvas [gl shader models]
  (gl/set-viewport gl (gl/get-viewport-rect gl))
  (gl/cull-faces gl glc/back)
  (gl/clear-color-and-depth-buffer gl 0 0 0 1 1)
  (doseq [model models]
    (gl/draw-with-shader gl (-> model
                                (gl/make-buffers-in-spec gl glc/static-draw)
                                (update :uniforms merge (spherical (gl/get-viewport-rect gl)))
                                (assoc :shader shader)))))

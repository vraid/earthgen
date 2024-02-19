(ns earthgen.graphics.core
  (:require [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.shaders :as sh]
            [thi.ng.geom.gl.shaders.basic :as sh-basic]
            [thi.ng.geom.gl.webgl.constants :as glc]))

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

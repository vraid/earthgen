(ns earthgen.graphics.core
  (:require [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.shaders :as sh]
            [thi.ng.geom.gl.shaders.basic :as sh-basic]
            [thi.ng.geom.gl.webgl.constants :as glc]))

(def default-shader (sh-basic/make-shader-spec-3d true))

(defn make-shader [gl]
  (sh/make-shader-from-spec gl default-shader))

(defn draw-canvas [gl buffers]
  (gl/set-viewport gl (gl/get-viewport-rect gl))
  (gl/cull-faces gl glc/back)
  (gl/clear-color-and-depth-buffer gl 0 0 0 1 1)
  (doseq [buffer buffers]
    (gl/draw-with-shader gl buffer)))

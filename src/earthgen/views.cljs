(ns earthgen.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]
            [reagent.dom :as rdom]
            [thi.ng.geom.gl.core :as gl]
            [earthgen.subs :as subs]
            [earthgen.events :as events]
            [earthgen.input :as input]
            [earthgen.graphics.core :as graphics]))

(defn canvas-inner []
  (let [mount (fn [canvas]
                (let
                 [shader (-> canvas rdom/dom-node gl/gl-context graphics/make-shader)]
                  (re-frame/dispatch [::events/set-shader shader])))
        update (fn [canvas]
                 (let
                  [props (reagent/props canvas)
                   gl (gl/gl-context (rdom/dom-node canvas))
                   perspective (graphics/spherical (:perspective props) (gl/get-viewport-rect gl))]
                   (graphics/draw-canvas gl (:shader props) perspective (:models props))))]
    (reagent/create-class
     {:reagent-render (fn []
                        [:canvas {:width 1000
                                  :height 1000
                                  :on-mouse-down input/mouse-down
                                  :on-touch-start input/touch-start
                                  :on-touch-end input/touch-end
                                  :on-touch-move input/touch-move
                                  :on-touch-cancel input/touch-cancel
                                  :style {:display "block"}}])
      :component-did-mount mount
      :component-did-update update
      :display-name "gl-canvas"})))

(defn canvas-outer []
  (let [data (re-frame/subscribe [::subs/graphics])]
    [canvas-inner @data]))

(defn main-panel []
  [:div {:on-mouse-up input/mouse-up
         :on-mouse-move input/mouse-move}
   [canvas-outer]])

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
                   perspective (:perspective props)
                   gl (gl/gl-context (rdom/dom-node canvas))
                   camera ((:camera perspective) perspective (gl/get-viewport-rect gl))]
                   (graphics/draw-canvas gl (:shader props) camera (:models props))))]
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

(defn gettext [e] (-> e .-target .-value))

(def button-style
  {:padding "8px 8px"
   :margin "4px 4px"})

(defn view-section [view]
  [:div
   [:h3 "View"]
   [:div
    [:label "Projection "]
    [:select.form-control
     {:style button-style
      :field :list
      :id :projection-input
      :value (get-in view [:perspectives (:current-perspective view) :name])
      :on-change (fn [e] (re-frame/dispatch [::events/set-perspective (get {"Spherical" :spherical "Hammer" :hammer} (gettext e))]))}
     [:option {:key :spherical} "Spherical"]
     [:option {:key :hammer} "Hammer"]]]])

(defn main-panel []
  (let
   [view @(re-frame/subscribe [::subs/view])]
    [:div {:on-mouse-up input/mouse-up
           :on-mouse-move input/mouse-move}
     [view-section view]
     [canvas-outer]]))

(ns earthgen.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]
            [reagent.dom :as rdom]
            [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.webgl.constants :as glc]
            [earthgen.subs :as subs]
            [earthgen.events :as events]
            [earthgen.input :as input]
            [earthgen.graphics.core :as graphics]
            [earthgen.math.quaternion :as quaternion]))

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
                   camera ((:camera perspective) perspective (gl/get-viewport-rect gl))
                   models (:models props)
                   shader (:shader props)
                   buffers (:buffers props)
                   nil-buffers? (not buffers)
                   buffers (if nil-buffers?
                             (mapv (fn [model]
                                     (gl/make-buffers-in-spec model gl glc/static-draw))
                                   models)
                             buffers)
                   transform-buffer (fn [buffer]
                                      (-> buffer
                                          (update :uniforms merge camera)
                                          (assoc :shader shader)))]
                   (graphics/draw-canvas gl (mapv transform-buffer buffers))
                   (when nil-buffers? (re-frame/dispatch [::events/set-buffers buffers]))))]
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

(def section-delimiter [[:h3]
                        [:div {:style {:border-bottom "1px solid black"
                                       :width "350px"}}]
                        [:h3]])

(defn custom-terrain-mode [generate model view update-input button]
  (let
   [to-input #(js->clj (.parse js/JSON (:custom-terrain view)) :keywordize-keys true)
    valid? (try (to-input)
                (catch js/Object _ false))]
    [:div
     [:div
      [:h3]
      [:b "Output"]
      [:div [:sub "Currently generated terrain. Copy to save or share"]]
      [:textarea {:cols 40
                  :rows 8
                  :read-only true
                  :value (.stringify js/JSON (clj->js model))}]]
     [:div
      [:h3]
      [:b "Input"]
      [:div [:sub "Paste a previous result to recreate"]]
      [:textarea {:cols 40
                  :rows 8
                  :value (:custom-terrain view)
                  :on-change (update-input [:custom-terrain])}]]
     (if valid?
       [:div
        (button "Generate" (generate to-input))]
       [:div "Invalid JSON"])]))

(defn predefined-terrain-mode [options]
  [:div
   [:h3]
   [:div "Press a button to generate a planet of that type"]
   [:div "Results will trend towards description, but may sometimes diverge"]
   (into [:div] options)])

(defn to-option [[k label]]
  [:option {:key k} label])

(defn view-section [perspectives current-value]
  [:div
   [:label "Projection "]
   (into
    [:select.form-control
     {:style button-style
      :field :list
      :id :projection-input
      :value current-value
      :on-change (fn [e]
                   (re-frame/dispatch
                    [::events/set-perspective
                     (get (into {} (map (comp vec reverse) perspectives)) (gettext e))]))}]
    (map to-option perspectives))])

(def tabs
  (let
   [tab-button (let
                [common-style {:border-radius "4px 4px 0 0"
                               :padding "10px"}]
                 (fn [active? label on-click]
                   (if active?
                     [:button
                      {:style (into
                               common-style
                               {:border "1px solid black"
                                :border-bottom "1px solid white"
                                :z-index 2
                                :background-color :transparent})}
                      label]
                     [:button
                      {:style (into
                               common-style
                               {:border "1px solid lightgray"
                                :border-bottom "1px solid black"})
                       :on-click on-click}
                      label])))]
    (fn [select-fn current items]
      (mapv (fn [item]
              (let
               [[option label] item]
                (tab-button (= option current) label (select-fn option))))
            items))))

(defn main-panel []
  (let
   [view @(re-frame/subscribe [::subs/view])
    terrain-mode (:terrain-mode view)
    model @(re-frame/subscribe [::subs/model])
    subdivisions (:subdivisions view)
    generate-model (fn [model]
                     (fn [_]
                       (re-frame/dispatch [::events/generate subdivisions (model)])))
    update-input (fn [keys]
                   (fn [e] (re-frame/dispatch
                            [::events/set-view (assoc-in view keys (gettext e))])))
    set-terrain-mode (fn [mode]
                       (fn [_]
                         (re-frame/dispatch
                          [::events/set-view (assoc view :terrain-mode mode)])))
    input (fn [keys]
            [:input {:type "text"
                     :value (get-in view keys)
                     :on-change (update-input keys)}])
    button (fn [label on-click]
             [:button
              {:style button-style
               :on-click on-click}
              label])]
    [:div
     [:h1 "Earthgen"]
     [:div {:style {:display :flex
                    :flex-wrap :wrap}
            :on-mouse-up input/mouse-up
            :on-mouse-move input/mouse-move}
      [:div {:style {:min-width "1050px"}}
       [canvas-outer]
       [:div (str "Current rotation [" (clj->js (quaternion/product (quaternion/conjugate (:current-rotation view)) (:planet-rotation view))) "]")]]
      (vec
       (concat
        [:div]
        [[:h3 "View settings"]]
        section-delimiter
        [[view-section
          (:perspectives view)
          (get-in view [(:current-perspective view) :name])]]
        [[:h3 "Generation"]]
        section-delimiter
        [[:b "Grid"]
         [:h3]
         [:div
          "Subdivisions "
          (input [:subdivisions])]
         [:div [:sup "[0, 1, 2 ...] Each increment roughly triples the polygon count"]]
         [:div [:sup (let
                      [parsed (parse-long subdivisions)
                       num (or (and parsed (max 0 parsed)) 0)]
                       (str num " "
                            (if (= 1 num) "subdivision" "subdivisions")
                            " will create "
                            (+ 2 (* 10 (Math/pow 3 num)))
                            " polygons"))]]
         [:div
          "Timeout (ms) "
          (input [:subdivision-timeout])]
         [:div [:sup "Limits grid size if subdivision takes too long. No limit if empty"]]]
        section-delimiter
        [[:b "Terrain"]
         [:h3]
         (into
          [:div]
          (tabs set-terrain-mode
                terrain-mode
                [[:predefined "Suggested"]
                 [:custom "Save / Load"]]))
         (case terrain-mode
           :predefined [predefined-terrain-mode
                        (map (fn [[k f]] (button k (generate-model f)))
                             (:predefined-terrain-options view))]
           :custom [custom-terrain-mode
                    generate-model
                    model
                    view
                    update-input
                    button]
           [:div ""])]))]]))

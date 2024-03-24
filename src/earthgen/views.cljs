(ns earthgen.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]
            [reagent.dom :as rdom]
            [thi.ng.geom.gl.core :as gl]
            [earthgen.subs :as subs]
            [earthgen.events :as events]
            [earthgen.input :as input]
            [earthgen.graphics.core :as graphics]
            [earthgen.generation.predefined :as predefined]))

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

(defn simple-mode [generate input button]
  [:div
   [:h3 " "]
   [:div
    "Seed "
    (input [:simple-terrain :seed])]
   [:sup "Any text. Each seed will give the same result across different devices"]
   [:div
    "Granularity "
    (input [:simple-terrain :granularity])]
   [:sup "[0, 1, 2 ...] Lower values result in larger oceans and continents"]
   [:div
    "Irregularity "
    (input [:simple-terrain :irregularity])]
   [:sup "0-1. Lower values result in a smoother topography"]
   [:div
    "Amplitude "
    (input [:simple-terrain :amplitude])]
   [:sup "Any number. Elevation scales linearly to this value"]
   [:div
    "Sea level "
    (input [:simple-terrain :sea-level])]
   [:sup "Floods the land"]
   [:div
    (button "Generate" generate)]])

(defn custom-mode [generate view update-input button]
  [:div
   [:h3]
   [:div "Input text to recreate a previous result,"]
   [:div "or change the parameters to try something new"]
   [:textarea {:cols 40
               :rows 8
               :value (:custom view)
               :on-change (update-input [:custom])}]
   [:div
    (button "Generate" (generate (js->clj (.parse js/JSON (:custom view)) :keywordize-keys true)))]])

(defn predefined-mode [generate button]
  [:div
   [:h3]
   [:div "Press a button to generate a planet of that type"]
   [:div "Results will trend towards description, but may sometimes diverge"]
   [:div
    (button "Continents" (generate (predefined/continents)))
    (button "Supercontinents" (generate (predefined/supercontinents)))
    (button "Archipelago" (generate (predefined/archipelago)))]])

(defn view-section [view]
  [:div
   [:h3 "View"]
   [:div
    [:label "Projection "]
    [:select.form-control
     {:style button-style
      :field :list
      :id :projection-input
      :value (get-in view [(:current-perspective view) :name])
      :on-change (fn [e] (re-frame/dispatch [::events/set-perspective (get {"Spherical" :spherical "Hammer" :hammer} (gettext e))]))}
     [:option {:key :spherical} "Spherical"]
     [:option {:key :hammer} "Hammer"]]]])

(defn main-panel []
  (let
   [view @(re-frame/subscribe [::subs/view])
    mode (:mode view)
    model @(re-frame/subscribe [::subs/model])
    subdivisions (:subdivisions view)
    generate-model (fn [model]
                     (fn [_]
                       (re-frame/dispatch [::events/generate subdivisions model])))
    update-input (fn [keys]
                   (fn [e] (re-frame/dispatch
                            [::events/set-view (assoc-in view keys (gettext e))])))
    set-mode (fn [mode]
               (fn [_]
                 (re-frame/dispatch
                  [::events/set-view (assoc view :mode mode)])))
    input (fn [keys]
            [:input {:type "text"
                     :value (get-in view keys)
                     :on-change (update-input keys)}])
    button (fn [label on-click]
             [:button
              {:style button-style
               :on-click on-click}
              label])]
    [:div {:on-mouse-up input/mouse-up
           :on-mouse-move input/mouse-move}
     [:h1 "Earthgen"]
     [:b "Grid"]
     [:div
      "Subdivisions "
      (input [:subdivisions])]
     [:div [:sup "[0, 1, 2 ...] Each increment roughly triples the polygon count. Recommended 6-8"]]
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
     [:div [:sup "Limits grid size if subdivision takes too long. No limit if empty"]]
     [:div
      [:b "Terrain : "]
      (if (= :predefined mode) "Suggested" (button "Suggested" (set-mode :predefined)))
      (if (= :simple mode) "Simple" (button "Simple" (set-mode :simple)))
      (if (= :custom mode) "Text input" (button "Text input" (set-mode :custom)))]
     (case mode
       :predefined [predefined-mode
                    generate-model
                    button]
       :simple [simple-mode
                (fn [_] (re-frame/dispatch [::events/generate-simple subdivisions (:simple-terrain view)]))
                input
                button]
       :custom [custom-mode
                generate-model
                view
                update-input
                button])
     [:h3]
     [:b "Output"]
     [:div [:sub "Copy-paste into the text input box to recreate"]]
     [:textarea {:cols 40
                 :rows 8
                 :read-only true
                 :value (.stringify js/JSON (clj->js model))}]
     [view-section view]
     [canvas-outer]]))

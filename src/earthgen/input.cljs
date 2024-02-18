(ns earthgen.input
  (:require [re-frame.core :as re-frame]
            [earthgen.events :as events]))

(defn bounding-rect [component]
  (let
   [rect (.getBoundingClientRect component)]
    [(.-x rect) (.-y rect) (.-width rect) (.-height rect)]))

(defn mouse-coord [e]
  [(.-clientX e) (.-clientY e)])

(defn mouse-extra [e]
  {:button (.-button e)
   :ctrl? (.-ctrlKey e)})

(defn mouse-down [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/mouse-down
                      (mouse-coord e)
                      (assoc (mouse-extra e)
                             :bounding-rect (bounding-rect (.-target e)))]))

(defn mouse-up [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/mouse-up
                      (mouse-coord e)
                      (mouse-extra e)]))

(defn mouse-move [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/mouse-move
                      (mouse-coord e)]))

(defn format-touches [ls]
  (let
   [count (.-length ls)
    vec (mapv #(.item ls %)
              (range count))]
    (reduce (fn [d a]
              (assoc d (.-identifier a) [(.-clientX a) (.-clientY a)]))
            {}
            vec)))

(defn changed-touches [e]
  (format-touches (.-changedTouches e)))

(defn touches [e]
  (format-touches (.-touches e)))

(defn touch-start [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/touch-start
                      (touches e)
                      (changed-touches e)
                      {:bounding-rect (bounding-rect (.-target e))}]))

(defn touch-end [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/touch-end
                      (touches e)
                      (changed-touches e)]))

(defn touch-move [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/touch-move
                      (touches e)
                      (changed-touches e)]))

(defn touch-cancel [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/touch-cancel
                      (touches e)
                      (changed-touches e)]))

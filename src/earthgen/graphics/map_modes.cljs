(ns earthgen.graphics.map-modes
  (:require [earthgen.interop.array :as js-array]))

(defn in-bands [bands]
  (let
   [[min-value max-value] [(js-array/first bands) (js-array/last bands)]
    stop-at (- (js-array/count bands) 2)]
    (fn [value]
      (let
       [a (max min-value (min max-value value))]
        (loop [n 0]
          (if (or (= n stop-at)
                  (<= a (js-array/get bands (inc n))))
            n
            (recur (inc n))))))))

(defn elevation [planet]
  (let
   [bands #js [-16000 -8000 -4000 -2000 -1000 -500 0 500 1000 2000 4000]
    colors (clj->js [[0 0 0.2 1]
                     [0 0.15 0.4 1]
                     [0 0.23 0.6 1]
                     [0.15 0.5 0.76 1]
                     [0.25 0.65 0.82 1]
                     [0.35 0.75 0.88 1]
                     [1 0.9 0.5 1]
                     [0.93 0.7 0.3 1]
                     [0.8 0.5 0.15 1]
                     [0.4 0.25 0 1]])
    sea-level (:sea-level planet)
    to-value (fn [a]
               (- a sea-level))
    elevation-in (fn [vec]
                   #(->> %
                         (js-array/get vec)
                         to-value))]
    [colors (comp (in-bands bands) (elevation-in (:tile-elevation planet)))]))

(defn solar-intensity [planet]
  (let
   [bands #js [0 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]
    colors (clj->js [[1 1 1 1]
                     [1 1 0 1]
                     [1 0.8 0 1]
                     [1 0.6 0 1]
                     [1 0.4 0 1]
                     [1 0.2 0 1]
                     [1 0 0 1]
                     [0.8 0 0 1]
                     [0.6 0 0 1]
                     [0.4 0 0 1]
                     [0.2 0 0 1]])
    get-in (fn [vec]
             #(js-array/get vec %))]
    [colors (comp (in-bands bands) (get-in (:potential-solar-radiation planet)))]))

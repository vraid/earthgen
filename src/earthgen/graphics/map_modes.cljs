(ns earthgen.graphics.map-modes
  (:require [earthgen.interop.array :as js-array]))

(defn in-bands [bands]
  (let
   [stop-at (- (js-array/count bands) 2)]
    (fn [a]
      (loop [n 0]
        (if (or (= n stop-at)
                (<= a (js-array/get bands (inc n))))
          (let
           [low (js-array/get bands n)
            high (js-array/get bands (inc n))]
            [n (/ (- a low) (- high low))])
          (recur (inc n)))))))

(defn elevation [planet]
  (let
   [bands #js [-16000 -8000 -4000 -2000 -1000 -200 0 500 1000 2000 4000]
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
    [min-value max-value] [(js-array/first bands) (js-array/last bands)]
    tile-elevation (:tile-elevation planet)
    corner-elevation (:corner-elevation planet)
    sea-level (:sea-level planet)
    to-value (fn [a]
               (- a sea-level))
    tile-value #(->> %
                     :id
                     (js-array/get tile-elevation)
                     to-value)
    corner-value #(->> %
                       :id
                       (js-array/get corner-elevation)
                       to-value)
    get-band (in-bands bands)
    color #(->> %
                (max min-value)
                (min max-value)
                get-band)]
    [bands
     colors
     (comp color tile-value)
     (comp color corner-value)]))

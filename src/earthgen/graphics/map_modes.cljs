(ns earthgen.graphics.map-modes
  (:require [earthgen.interop.array :as js-array]))

(defn in-bands [bands]
  (let
   [stop-at (- (js-array/count bands) 2)]
    (fn [a]
      (loop [n 0]
        (if (or (= n stop-at)
                (<= a (js-array/get bands (inc n))))
          n
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
    sea-level (:sea-level planet)
    to-value (fn [a]
               (- a sea-level))
    elevation-in (fn [vec]
                   #(->> %
                         (js-array/get vec)
                         to-value))
    get-band (in-bands bands)
    color #(->> %
                (max min-value)
                (min max-value)
                get-band)]
    [colors (comp color (elevation-in (:tile-elevation planet)))]))

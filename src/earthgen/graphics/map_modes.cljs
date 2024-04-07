(ns earthgen.graphics.map-modes
  (:require [earthgen.interop.array :as js-array]))

(defn elevation [planet]
  (let
   [tile-elevation (:tile-elevation planet)
    sea-level (:sea-level planet)]
    (fn [tile]
      (let
       [a (- (js-array/get tile-elevation (:id tile))
             sea-level)]
        (cond
          (> a 2000) #js [0.4 0.25 0 1]
          (> a 1000) #js [0.8 0.5 0.15 1]
          (> a 500) #js [0.93 0.7 0.3 1]
          (>= a 0) #js [1 0.9 0.5 1]
          (< a -8000) #js [0 0 0.2 1]
          (< a -4000) #js [0 0.15 0.4 1]
          (< a -2000) #js [0 0.23 0.6 1]
          (< a -1000) #js [0.15 0.5 0.76 1]
          (< a -200) #js [0.25 0.65 0.82 1]
          (< a 0) #js [0.35 0.75 0.88 1])))))

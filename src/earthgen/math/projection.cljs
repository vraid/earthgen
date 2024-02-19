(ns earthgen.math.projection
  (:refer-clojure :exclude [identity]))

(defn identity [_]
  (fn [[x y z]]
    #js [x y z]))

(def sqrt2 (Math/sqrt 2))

(defn hammer [[x y _]]
  (let
   [center-longitude (Math/atan2 y x)
    backside? (> (Math/abs center-longitude) (* 0.25 Math/PI))]
    (fn [[x y z]]
      (let
       [longitude (Math/atan2 y x)
        adj (if (and backside?
                     (> (Math/abs longitude) (* 0.25 Math/PI))
                     (> 0 (Math/sign (* longitude center-longitude))))
              (* (- (* 2 Math/PI) (Math/abs longitude))
                 (Math/sign center-longitude))
              longitude)
        sin-latitude z
        cos-latitude (Math/sqrt (- 1 (* z z)))
        sin-longitude (Math/sin (* 0.5 adj))
        cos-longitude (Math/cos (* 0.5 adj))
        scale (/ sqrt2 (Math/sqrt (+ 1 (* cos-latitude cos-longitude))))]
        #js [(* scale 2 cos-latitude sin-longitude)
             (* scale sin-latitude)
             0]))))

(ns earthgen.math.spherical)

(defn latitude [[_ _ z]]
  (Math/asin z))

(defn longitude [[x y _]]
  (Math/atan2 y x))

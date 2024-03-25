(ns earthgen.math.trigonometry)

(defn triangle-area [a b c]
  (let
   [semi-perimeter (* 0.5 (+ a b c))
    sub (partial - semi-perimeter)]
    (Math/sqrt (* semi-perimeter (sub a) (sub b) (sub c)))))

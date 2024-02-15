(ns earthgen.math.quaternion
  (:refer-clojure :exclude [identity])
  (:require [earthgen.math.vector :as vector]))

(def identity [1 0 0 0])

(def normal vector/normal)

(defn conjugate [q]
  (let
   [[a b c d] q]
    [a (- b) (- c) (- d)]))

(defn product [p q]
  (let
   [[a b c d] q]
    (mapv (fn [[sign q]]
            (reduce + 0 (map * p q sign)))
          [[[1 -1 -1 -1]
            [a  b  c  d]]
           [[1  1  1 -1]
            [b  a  d  c]]
           [[1 -1  1  1]
            [c  d  a  b]]
           [[1  1 -1  1]
            [d  c  b  a]]])))

(def product-normal (comp normal product))

(defn vector-product [q v]
  (vec (rest (reduce product [q (cons 0 v) (conjugate q)]))))

(defn from-axis-angle [axis angle]
  (let
   [a (* 0.5 angle)]
    (normal (cons (Math/cos a)
                  (vector/scale-by (Math/sin a)
                                   (vector/normal axis))))))

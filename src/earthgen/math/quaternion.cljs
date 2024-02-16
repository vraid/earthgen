(ns earthgen.math.quaternion
  (:refer-clojure :exclude [identity])
  (:require [earthgen.math.vector :as vector]
            [earthgen.math.matrix :as matrix]))

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

(defn to-matrix [q]
  (let
   [[a b c d] q
    [b2 c2 d2] (map (fn [n] (* n n)) [b c d])
    [ab ac ad] (map (partial * a) [b c d])
    bd (* b d)
    bc (* b c)
    cd (* c d)]
    (matrix/sum
     (matrix/identity 3)
     (matrix/scale-by 2
                      [[(- (+ c2 d2)) (- bc ad) (+ bd ac)]
                       [(+ bc ad) (- (+ b2 d2)) (- cd ab)]
                       [(- bd ac) (+ cd ab) (- (+ b2 c2))]]))))

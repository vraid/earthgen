(ns earthgen.generation.operations
  (:refer-clojure :exclude [min max abs])
  (:require [earthgen.generation.core :as generation]))

(defn op [k]
  (fn [& args]
    (into [k] args)))

(def op+ (op "+"))
(def op* (op "*"))
(def op- (op "-"))
(def min (op "min"))
(def max (op "max"))

(defn abs [a]
  ["abs" a])

(defn sigmoid [args a]
  ["sigmoid" args a])

(defn sigmoid-at [[min max] [y1 x1] [y2 x2] a]
  (let
   [z (fn [y] (Math/log (- (/ 1 y) 1)))
    z1 (z y1)
    z2 (z y2)
    k (/ (- z2 z1) (- x1 x2))
    p (+ x1 (/ z1 k))]
    (sigmoid {:min min :max max :scale k :offset p} a)))

(defn heightmap [args]
  ["heightmap" args])

(defn terrain [seed sea-level method]
  [["with-seed" (if seed {:seed seed} {})]
   method
   ["with-sea-level" {:sea-level sea-level}]
   ["with-rotation" {:rotation generation/default-rotation}]])

(defn heightmap-let [values final]
  ["heightmap-let" {}
   values
   final])

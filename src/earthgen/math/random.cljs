(ns earthgen.math.random
  (:refer-clojure :exclude [next take])
  (:require ["seedrandom" :as sr]
            [earthgen.interop.array :as js-array]))

(defn reseed [state]
  (sr "" (clj->js {:state state})))

(defn with-seed [seed]
  (.state (sr seed (clj->js {:state true}))))

(defn next [state]
  (let
   [rng (reseed state)
    a (rng)]
    [(.state rng) a]))

(defn take [n state]
  (let
   [rng (reseed state)
    result (js-array/build n rng)]
    [(.state rng) result]))

(defn from-list [ls a]
  (nth ls (Math/floor (* (count ls) a))))

(defn in [state ls]
  (let
   [rng (reseed state)
    a (rng)]
    [(.state rng) (from-list ls a)]))

(def to-angle
  (partial * 2 Math/PI))

(defn on-sphere [a b]
  (let
   [z (- (* 2 a) 1)
    xy (Math/sqrt (- 1 (* z z)))
    angle (to-angle b)]
    [(* xy (Math/cos angle)) (* xy (Math/sin angle)) z]))

(def seed-characters "0123456789abcdefghijklmnopqrstuvwxyz")

(defn random-seed [n]
  (apply str (map (fn [_]
                    (from-list seed-characters (Math/random)))
                  (range n))))

(defn pseudo-random-seed [n rng]
  (let
   [[rng ls] (take n rng)]
    [rng (apply str (map
                     (partial from-list seed-characters)
                     ls))]))

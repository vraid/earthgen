(ns earthgen.generation.generic
  (:require [earthgen.interop.array :as js-array]
            [earthgen.math.random :as random]
            [earthgen.generation.core :as generation]
            [earthgen.generation.terrain :as terrain]))

(declare from-input)

(defn vector+ [a b] (js-array/map (fn [a b] (+ a b)) a b))
(defn vector- [a] (js-array/map (fn [a] (- a)) a))
(defn vector* [a b] (js-array/map (fn [a b] (* a b)) a b))
(defn vector-max [a b] (js-array/map (fn [a b] (max a b)) a b))
(defn vector-min [a b] (js-array/map (fn [a b] (min a b)) a b))

; guard against (Math/exp 700) returning infinity
(defn sigmoid [{:keys [min max scale offset]}]
  (fn [a]
    (let
     [x (* scale (- a offset))]
      (if (< x -700)
        min
        (+ min
           (/ (- max min) (+ 1 (Math/exp (- x)))))))))

(defn wrap-rng [f]
  (fn [rng args]
    [rng (f args)]))

(def elevation-env
  (into {}
        (map (fn [[k f]] [k (wrap-rng f)])
             {"+" (fn [args] (reduce vector+ args))
              "-" (fn [args] ((if (= 1 (count args)) identity vector-)
                              (reduce vector+ (vector- (first args)) (rest args))))
              "*" (fn [args] (reduce vector* args))
              "pow" (fn [[a b]] (js-array/map (fn [a b] (Math/pow a b)) a b))
              "abs" (fn [[a]] (js-array/map (fn [a] (Math/abs a)) a))
              "max" (fn [args] (reduce vector-max args))
              "min" (fn [args] (reduce vector-min args))
              "sigmoid" (fn [[args a]] (let [f (sigmoid args)]
                                         (js-array/map (fn [a] (f a)) a)))})))

(defn eval-expr [env n]
  (fn [rng a]
    (cond
      (number? a) [rng (js-array/make n a)]
      (string? a) [rng (get env a)]
      (map? a) [rng a]
      (seq a) (let
               [f (eval-expr env n)
                [rng args] (reduce
                            (fn [[rng res] expr]
                              (let
                               [[rng a] (f rng expr)]
                                [rng (conj res a)]))
                            [rng []]
                            a)]
                ((first args) rng (rest args))))))

(defn heightmap-let [_ values expr]
  (fn [grids]
    (fn [[rng planet]]
      (let
       [tile-count (js-array/count (:tile-vertices planet))
        corner-count (js-array/count (:corner-vertices planet))
        last-grid (last grids)
        elevation-vector (fn [a] (js-array/concat (:tile-elevation a) (:corner-elevation a)))
        total-count (+ tile-count corner-count)
        env (assoc elevation-env
                   "heightmap"
                   (fn [rng [args]]
                     (let
                      [[rng a] (((apply terrain/heightmap (map args [:granularity :irregularity :amplitude :seed])) grids)
                                [rng last-grid])]
                       [rng (elevation-vector a)])))
        [rng env] (reduce (fn [[rng env] [k expr]]
                            (let
                             [[rng value] ((eval-expr env total-count) rng expr)]
                              [rng (assoc env k value)]))
                          [rng env]
                          values)
        [rng result] ((eval-expr env total-count) rng expr)]
        [rng
         (assoc planet
                :tile-elevation (js-array/take tile-count result)
                :corner-elevation (js-array/drop tile-count result))]))))

(def transforms
  {"with-seed" (fn [[args]] (generation/with-seed (:seed args)))
   "heightmap" (fn [[args]] (apply terrain/heightmap (map args [:granularity :irregularity :amplitude :seed])))
   "heightmap-let" (fn [[args values formula]] (heightmap-let args values formula))
   "with-sea-level" (fn [[args]] (terrain/sea-level (:sea-level args)))
   "with-rotation" (fn [[args]] (generation/with-rotation (:rotation args)))
   "with-radius" (fn [[args]] (generation/with-radius (:radius args)))
   "axial-tilt" (fn [[args]] (generation/axial-tilt (:tilt args)))
   "orbital-position" (fn [[args]] (generation/orbital-position (:position args)))
   "solar-intensity" (fn [[args]] (generation/solar-intensity (:intensity args)))})

(defn input-transforms [ls]
  (mapv (fn [a]
          ((get transforms (first a)) (rest a)))
        ls))

(defn from-input [ls]
  (let
   [seed (random/random-seed 12)]
    (cons (let
           [[k args] (first ls)]
            (if (= "with-seed" k)
              [k (update args :seed (fn [a] (or a seed)))]
              [k args]))
          (rest ls))))

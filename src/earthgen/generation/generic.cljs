(ns earthgen.generation.generic
  (:require [earthgen.math.random :as random]
            [earthgen.generation.core :as generation]
            [earthgen.generation.terrain :as terrain]))

(declare from-input)

(defn vector+ [a b] (mapv + a b))
(defn vector- [a] (mapv - a))
(defn vector* [a b] (mapv * a b))
(defn vector-max [a b] (mapv max a b))
(defn vector-min [a b] (mapv min a b))

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
              "-" (fn [args] (mapv (if (= 1 (count args)) identity -)
                                   (reduce vector+ (vector- (first args)) (rest args))))
              "*" (fn [args] (reduce vector* args))
              "pow" (fn [[a b]] (mapv Math/pow a b))
              "abs" (fn [[a]] (mapv Math/abs a))
              "max" (fn [args] (reduce vector-max args))
              "min" (fn [args] (reduce vector-min args))
              "sigmoid" (fn [[args a]] (mapv (sigmoid args) a))})))

(defn eval-expr [env n]
  (fn [rng a]
    (cond
      (number? a) [rng (vec (repeat n a))]
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
       [tile-count (count (:tiles planet))
        corner-count (count (:corners planet))
        last-grid (last grids)
        elevation-vector (fn [a] (into (:tile-elevation a) (:corner-elevation a)))
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
                :tile-elevation (vec (take tile-count result))
                :corner-elevation (vec (take tile-count result)))]))))

(def transforms
  {"with-seed" (fn [[args]] (generation/with-seed (:seed args)))
   "heightmap" (fn [[args]] (apply terrain/heightmap (map args [:granularity :irregularity :amplitude :seed])))
   "heightmap-let" (fn [[args values formula]] (heightmap-let args values formula))
   "with-sea-level" (fn [[args]] (terrain/sea-level (:sea-level args)))
   "with-rotation" (fn [[args]] (generation/with-rotation (:rotation args)))
   "with-radius" (fn [[args]] (generation/with-radius (:radius args)))})

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

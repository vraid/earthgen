(ns earthgen.generation.core
  (:require [earthgen.math.random :as random]
            [earthgen.grid.core :as grid]
            [earthgen.math.vector :as vector]
            [earthgen.math.spherical :as spherical]
            [earthgen.astronomy.sunlight :as sunlight]
            [earthgen.math.matrix :as matrix]
            [earthgen.math.quaternion :as quaternion]))

(defn timed-subdivision [[_ grid]]
  (let
   [start (system-time)
    subdivided (grid/subdivide grid)]
    [(- (system-time) start) subdivided]))

(defn grids-with-timeout [grids timeout subdivisions]
  (if (not timeout)
    [subdivisions (mapv second (take (inc subdivisions) grids))]
    (loop [time 0
           n 0]
      (let
       [taken (doall (take (inc n) grids))
        [t _] (last taken)
        total (+ time t)]
        (if (or (= n subdivisions)
                (< timeout total))
          [n (mapv second taken)]
          (recur total (inc n)))))))

(defn with-seed [seed]
  (fn [_]
    (fn [[rng result]]
      [(if seed (random/with-seed seed) rng) result])))

(def default-rotation [1 0 0 0])

(defn with-rotation [rotation]
  (let
   [normal (vector/normal rotation)
    valid? (and (not (vector/invalid? normal))
                (= 4 (count normal)))]
    (fn [_]
      (fn [[rng result]]
        (and valid? [rng (assoc result :rotation normal)])))))

(defn with-radius [radius]
  (fn [_]
    (fn [[rng result]]
      [rng (assoc result :radius radius)])))

(defn axial-tilt [tilt]
  (fn [_]
    (fn [[rng result]]
      [rng (assoc result :axial-tilt tilt)])))

(defn orbital-position [position]
  (fn [_]
    (fn [[rng result]]
      [rng (assoc result :orbital-position position)])))

(defn solar-intensity [intensity]
  (fn [_]
    (fn [[rng result]]
      (let
       [axial-tilt (:axial-tilt result)
        orbital-position (:orbital-position result)
        latitude (comp spherical/latitude
                       (matrix/vector-product (quaternion/to-matrix (:rotation result)))
                       :center)
        solar-declination (* axial-tilt (Math/sin orbital-position))
        potential-radiation (comp (partial * intensity)
                                  (sunlight/potential-solar-radiation solar-declination)
                                  latitude)]
        [rng (assoc result :potential-solar-radiation (mapv potential-radiation (:tiles result)))]))))

(defn transform [grids ls]
  (reduce (fn [result f]
            ((f grids) result))
          [(random/with-seed (random/random-seed 256)) (last grids)]
          (cons (with-rotation default-rotation)
                ls)))

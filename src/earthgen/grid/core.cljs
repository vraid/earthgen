(ns earthgen.grid.core
  (:require [earthgen.interop.array :as js-array]
            [earthgen.grid.icosahedron :as icosahedron]
            [earthgen.math.trigonometry :as trig]))

(defn get-tile [tiles]
  (partial js-array/get tiles))

(defn tile-center [tiles]
  (comp :center (get-tile tiles)))

(defn pairwise-base [f]
  (fn [initial ls]
    (let
     [count (js-array/count ls)
      nth (fn [n] (js-array/get ls (mod n count)))]
      (js-array/build count
                      (fn [n]
                        (let
                         [a (nth n)
                          b (nth (inc n))]
                          (if initial (f initial a b) (f a b))))))))

(defn to-vec ([a b] [a b]) ([a b c] [a b c]))
(def pairwise (pairwise-base to-vec))

(defn square [a] (* a a))

(defn midpoint [[x1 y1 z1]]
  (fn [[x2 y2 z2]
       [x3 y3 z3]]
    (let
     [x (+ x1 x2 x3)
      y (+ y1 y2 y3)
      z (+ z1 z2 z3)
      scale (/ 1 (Math/sqrt (+ (square x) (square y) (square z))))]
      #js [(* scale x) (* scale y) (* scale z)])))

(defn create-corners [tile-vec old-tiles]
  (let
   [tile-center (tile-center tile-vec)]
    (persistent!
     (loop [n 0
            ls (vec old-tiles)
            result (transient [])]
       (if (empty? ls)
         result
         (let
          [{:keys [id center tiles]} (first ls)
           midpoint (midpoint center)
           [n result] (loop [n n
                             ls (vec (pairwise false tiles))
                             result result]
                        (if (empty? ls)
                          [n result]
                          (let
                           [[a b] (first ls)
                            corner (and (< id a)
                                        (< id b)
                                        {:id n
                                         :tiles #js [id a b]
                                         :vertex (midpoint (tile-center a) (tile-center b))})]
                            (if corner
                              (recur (inc n) (rest ls) (conj! result corner))
                              (recur n (rest ls) result)))))]
           (recur n (rest ls) result)))))))

(defn distance [a b]
  (let
   [nth (fn [n]
          (square (- (aget a n) (aget b n))))]
    (Math/sqrt (+ (nth 0) (nth 1) (nth 2)))))

(defn tile-area [tile corners]
  (let
   [center (:center tile)
    count (js-array/count corners)
    vertices (js-array/map :vertex corners)
    distances (js-array/map (fn [v] (distance center v)) vertices)
    vertex (fn [n] (js-array/get vertices (mod n count)))
    dist (fn [n] (js-array/get distances (mod n count)))]
    (reduce (fn [area n]
              (+ area
                 (trig/triangle-area
                  (distance (vertex n) (vertex (inc n)))
                  (dist n)
                  (dist (inc n)))))
            0.0
            (range count))))

(defn tile-with-area [corner-vec]
  (fn [a]
    (assoc a :area (tile-area a (js-array/map (fn [n]
                                                (js-array/get corner-vec n))
                                              (:corners a))))))

(defn tile-with-corners [corner-vec]
  (let
   [dict (into {}
               (map (fn [{:keys [tiles id]}] [(sort tiles) id]))
               corner-vec)
    tile-corners (fn [ids] (get dict (sort ids)))]
    (fn [{:keys [id tiles] :as a}]
      (let
       [corners (js-array/map tile-corners
                              (pairwise id tiles))]
        (assoc a :corners (js-array/shift 1 corners))))))

(defn corner-with-corners [tile-vec]
  (fn [{:keys [id tiles] :as a}]
    (let
     [next-corner (fn [tile]
                    (let
                     [corners (:corners (js-array/get tile-vec tile))
                      n (mod (inc (.indexOf corners id))
                             (js-array/count corners))]
                      (js-array/get corners n)))]
      (assoc a :corners (js-array/map next-corner tiles)))))

(defn grid-from-tiles [old-tiles new-tiles]
  (let
   [tile-vec (js-array/concat old-tiles new-tiles)
    corner-vec (create-corners tile-vec old-tiles)
    corner-arr (apply array corner-vec)
    final-tiles (js-array/map (comp (tile-with-area corner-arr)
                                    (tile-with-corners corner-vec))
                              tile-vec)
    final-corners (js-array/map (corner-with-corners final-tiles) corner-arr)]
    {:tiles final-tiles
     :corners final-corners}))

(defn subdivide [grid]
  (let
   [tiles (:tiles grid)
    tile-count (js-array/count tiles)
    new-tile-id (partial + tile-count)
    corners (:corners grid)]
    (grid-from-tiles
     (js-array/map (fn [a]
                     {:id (:id a)
                      :center (:center a)
                      :tiles (js-array/map new-tile-id (:corners a))})
                   tiles)
     (js-array/map (fn [corner]
                     {:id (new-tile-id (:id corner))
                      :center (:vertex corner)
                      :tiles (let
                              [[b d f] (:tiles corner)
                               [a c e] (js-array/map new-tile-id (:corners corner))]
                               #js [a b c d e f])})
                   corners))))

(defn initial []
  (grid-from-tiles
   (apply array
          (mapv (fn [id vertex tiles]
                  {:id id
                   :center vertex
                   :tiles (apply array tiles)})
                (range 12)
                icosahedron/vertices
                icosahedron/indices))
   (array)))

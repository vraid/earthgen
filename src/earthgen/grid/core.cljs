(ns earthgen.grid.core
  (:require [earthgen.grid.icosahedron :as icosahedron]
            [earthgen.math.vector :as vector]
            [earthgen.math.trigonometry :as trig]))

(defn get-tile [tiles]
  (partial nth tiles))

(defn tile-center [tiles]
  (comp :center (get-tile tiles)))

(defn pairwise [initial ls]
  (let
   [last (first ls)]
    (persistent!
     (loop [a last
            ls (rest ls)
            result (transient [])]
       (if (empty? ls)
         (conj! result (if initial [initial a last] [a last]))
         (let
          [b (first ls)]
           (recur b (rest ls) (conj! result (if initial [initial a b] [a b])))))))))

(defn square [a] (* a a))

(defn midpoint [[x1 y1 z1]]
  (fn [[x2 y2 z2]
       [x3 y3 z3]]
    (let
     [x (+ x1 x2 x3)
      y (+ y1 y2 y3)
      z (+ z1 z2 z3)
      scale (/ 1 (Math/sqrt (+ (square x) (square y) (square z))))]
      [(* scale x) (* scale y) (* scale z)])))

(defn create-corners [tile-vec old-tiles]
  (let
   [tile-center (tile-center tile-vec)]
    (persistent!
     (loop [n 0
            ls old-tiles
            result (transient [])]
       (if (empty? ls)
         result
         (let
          [{:keys [id center tiles]} (first ls)
           midpoint (midpoint center)
           [n result] (loop [n n
                             ls (pairwise false tiles)
                             result result]
                        (if (empty? ls)
                          [n result]
                          (let
                           [[a b] (first ls)
                            corner (and (< id a)
                                        (< id b)
                                        {:id n
                                         :tiles [id a b]
                                         :vertex (midpoint (tile-center a) (tile-center b))})]
                            (if corner
                              (recur (inc n) (rest ls) (conj! result corner))
                              (recur n (rest ls) result)))))]
           (recur n (rest ls) result)))))))

(defn tile-area [tile corners]
  (let
   [center (:center tile)
    vertices (mapv :vertex corners)
    distances (mapv (partial vector/distance center) vertices)
    last-vec (first vertices)
    last-dist (first distances)]
    (loop [area 0.0
           avec (first vertices)
           vecs (rest vertices)
           adist (first distances)
           dists (rest distances)]
      (let
       [[bvec bdist] (if (empty? vecs) [last-vec last-dist] [(first vecs) (first dists)])
        area (+ area (trig/triangle-area (vector/distance avec bvec) adist bdist))]
        (if (empty? vecs)
          area
          (recur area bvec (rest vecs) bdist (rest dists)))))))

(defn tile-with-corners [corner-vec]
  (let
   [dict (into {}
               (map (fn [{:keys [tiles id]}] [(sort tiles) id]))
               corner-vec)
    tile-corners (fn [ids] (get dict (sort ids)))]
    (fn [{:keys [id tiles] :as a}]
      (let
       [corners (mapv tile-corners
                      (pairwise id tiles))]
        (assoc a
               :corners corners
               :area (tile-area a (mapv (partial nth corner-vec) corners)))))))

(defn corner-with-corners [tile-vec]
  (fn [{:keys [id tiles] :as a}]
    (let
     [next-corner (fn [tile]
                    (let
                     [corners (:corners (nth tile-vec tile))
                      n (mod (inc (.indexOf corners id))
                             (count corners))]
                      (nth corners n)))]
      (assoc a :corners (mapv next-corner tiles)))))

(defn grid-from-tiles [old-tiles new-tiles]
  (let
   [tile-vec (into old-tiles new-tiles)
    corner-vec (create-corners tile-vec old-tiles)
    final-tiles (mapv (tile-with-corners corner-vec) tile-vec)
    final-corners (mapv (corner-with-corners final-tiles) corner-vec)]
    {:tiles (apply array final-tiles)
     :corners (apply array final-corners)}))

(defn subdivide [grid]
  (let
   [tiles (vec (:tiles grid))
    tile-count (count tiles)
    new-tile-id (partial + tile-count)
    corners (vec (:corners grid))]
    (grid-from-tiles
     (mapv (fn [a]
             {:id (:id a)
              :center (:center a)
              :tiles (mapv new-tile-id (:corners a))})
           tiles)
     (mapv (fn [corner]
             {:id (new-tile-id (:id corner))
              :center (:vertex corner)
              :tiles (let
                      [[b d f] (:tiles corner)
                       [a c e] (mapv new-tile-id (:corners corner))]
                       [a b c d e f])})
           corners))))

(defn initial []
  (grid-from-tiles
   (mapv (fn [id vertex tiles]
           {:id id
            :center vertex
            :tiles tiles})
         (range 12)
         icosahedron/vertices
         icosahedron/indices)
   []))

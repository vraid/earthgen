(ns earthgen.grid.core
  (:require [earthgen.math.vector :as vector]
            [earthgen.grid.icosahedron :as icosahedron]))

(defn get-tile [tiles]
  (partial nth tiles))

(defn tile-center [tiles]
  (comp :center (get-tile tiles)))

(defn pairwise [initial ls]
  (mapv (fn [a b]
          (if initial [initial a b] [a b]))
        ls
        (conj (vec (rest ls)) (first ls))))

(defn create-corners [tile-vec old-tiles]
  (let
   [tile-center (tile-center tile-vec)]
    (map-indexed
     (fn [id a] (assoc a :id id))
     (mapcat
      (fn [{:keys [id center tiles]}]
        (mapcat
         (fn [[a b]]
           (let
            [corner (and (< id a)
                         (< id b)
                         {:tiles [id a b]
                          :vertex (vector/normal (reduce vector/sum center (map tile-center [a b])))})]
             (if corner [corner] [])))
         (pairwise false tiles)))
      old-tiles))))

(defn tile-with-corners [corner-vec]
  (let
   [dict (into {}
               (map (fn [{:keys [tiles id]}] [(sort tiles) id]))
               corner-vec)
    tile-corners (fn [ids] (get dict (sort ids)))]
    (fn [{:keys [id tiles] :as a}]
      (assoc a :corners (mapv tile-corners
                              (pairwise id tiles))))))

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
    {:tiles final-tiles
     :corners final-corners}))

(defn subdivide [grid]
  (let
   [tiles (:tiles grid)
    tile-count (count tiles)
    new-tile-id (partial + tile-count)
    corners (:corners grid)]
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

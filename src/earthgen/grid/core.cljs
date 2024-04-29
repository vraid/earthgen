(ns earthgen.grid.core
  (:require [earthgen.interop.array :as js-array]
            [earthgen.grid.icosahedron :as icosahedron]
            [earthgen.math.trigonometry :as trig]))

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

(defn distance [a b]
  (let
   [nth (fn [a b] (square (- a b)))]
    (Math/sqrt (js-array/reduce + 0.0 (js-array/map nth a b)))))

(defn create-edges [tile-tiles corner-vertices tile-corners]
  (let
   [tile-count (js-array/count tile-tiles)
    edge-lengths (js-array/make (* 3 (- tile-count 2)) nil)
    tile-edges (js-array/build tile-count (fn [n] (js-array/make (if (< n 12) 5 6) -1)))
    get-tile-tiles (js-array/access tile-tiles)
    get-tile-edges (js-array/access tile-edges)]
    (loop [n 0
           id 0]
      (if (= id tile-count)
        [tile-edges edge-lengths]
        (let
         [tiles (get-tile-tiles id)
          corners (js-array/get tile-corners id)
          n (loop [edge-id n
                   ks (range (js-array/count tiles))]
              (if (empty? ks)
                edge-id
                (let
                 [k (first ks)
                  adj (js-array/get tiles k)]
                  (if (< id adj)
                    (let
                     [corner-vertex (fn [n] (js-array/get corner-vertices (js-array/get corners n)))
                      length (distance (corner-vertex k) (corner-vertex (mod (inc k) (js-array/count corners))))]
                      (aset (get-tile-edges id) k edge-id)
                      (aset (get-tile-edges adj) (.indexOf (get-tile-tiles adj) id) edge-id)
                      (aset edge-lengths edge-id length)
                      (recur (inc edge-id) (rest ks)))
                    (recur edge-id (rest ks))))))]
          (recur n (inc id)))))))

(defn midpoint [[x1 y1 z1]]
  (fn [[x2 y2 z2]
       [x3 y3 z3]]
    (let
     [x (+ x1 x2 x3)
      y (+ y1 y2 y3)
      z (+ z1 z2 z3)
      scale (/ 1 (Math/sqrt (+ (square x) (square y) (square z))))]
      #js [(* scale x) (* scale y) (* scale z)])))

(defn create-corners [tile-vertices tile-tiles old-tile-count]
  (let
   [tile-vertex (js-array/access tile-vertices)
    tile-corners (js-array/build (js-array/count tile-vertices) (fn [n] (js-array/make (if (< n 12) 5 6) -1)))
    corner-count (* 2 (- (js-array/count tile-vertices) 2))
    corner-tiles (js-array/make corner-count nil)
    corner-vertices (js-array/make corner-count nil)
    get-tile-tiles (js-array/access tile-tiles)]
    (loop [n 0
           id 0]
      (if (= id old-tile-count)
        [tile-corners corner-tiles corner-vertices]
        (let
         [tiles (get-tile-tiles id)
          midpoint (midpoint (tile-vertex id))
          n (loop [corner-id n
                   ls (seq (pairwise false tiles))]
              (if (empty? ls)
                corner-id
                (let
                 [[a b] (first ls)]
                  (if (and (< id a) (< id b))
                    (let
                     [set-in (fn [n adj]
                               (aset (js-array/get tile-corners n)
                                     (.indexOf (get-tile-tiles n) adj)
                                     corner-id))]
                      (set-in id b)
                      (set-in a id)
                      (set-in b a)
                      (aset corner-vertices corner-id (midpoint (tile-vertex a) (tile-vertex b)))
                      (aset corner-tiles corner-id #js [id a b])
                      (recur (inc corner-id) (rest ls)))
                    (recur corner-id (rest ls))))))]
          (recur n (inc id)))))))

(defn tile-area [corner-vertices edge-lengths]
  (fn [vertex corners edges]
    (let
     [count (js-array/count corners)
      distances (js-array/map (fn [n] (distance vertex (js-array/get corner-vertices n))) corners)
      dist (fn [n] (js-array/get distances (mod n count)))]
      (transduce (map (fn [n]
                        (trig/triangle-area
                         (js-array/get edge-lengths (js-array/get edges n))
                         (dist n)
                         (dist (inc n)))))
                 +
                 0.0
                 (range count)))))

(defn corner-with-corners [tile-corners]
  (fn [id tiles]
    (let
     [next-corner (fn [tile]
                    (let
                     [corners (js-array/get tile-corners tile)
                      n (mod (inc (.indexOf corners id))
                             (js-array/count corners))]
                      (js-array/get corners n)))]
      (js-array/map next-corner tiles))))

(defn grid-from-tiles [tile-vertices old-tile-tiles new-tile-tiles]
  (let
   [tile-tiles (js-array/concat old-tile-tiles new-tile-tiles)
    [tile-corners corner-tiles corner-vertices] (create-corners tile-vertices tile-tiles (js-array/count old-tile-tiles))
    [tile-edges edge-lengths] (create-edges tile-tiles corner-vertices tile-corners)
    tile-areas (js-array/map (tile-area corner-vertices edge-lengths)
                             tile-vertices
                             tile-corners
                             tile-edges)
    corner-corners (js-array/map-indexed (corner-with-corners tile-corners)
                                         corner-tiles)]
    {:tile-vertices tile-vertices
     :tile-tiles tile-tiles
     :tile-corners tile-corners
     :tile-edges tile-edges
     :tile-areas tile-areas
     :corner-vertices corner-vertices
     :corner-tiles corner-tiles
     :corner-corners corner-corners
     :edge-lengths edge-lengths}))

(defn subdivide [grid]
  (let
   [tile-count (js-array/count (:tile-vertices grid))
    new-tile-id (partial + tile-count)]
    (grid-from-tiles
     (js-array/concat (:tile-vertices grid) (:corner-vertices grid))
     (js-array/map #(js-array/map new-tile-id %)
                   (:tile-corners grid))
     (js-array/map (fn [tiles corners]
                     (let
                      [[b d f] tiles
                       [a c e] (map new-tile-id corners)]
                       #js [a b c d e f]))
                   (:corner-tiles grid)
                   (:corner-corners grid)))))

(defn initial []
  (grid-from-tiles
   (clj->js icosahedron/vertices)
   (clj->js icosahedron/indices)
   (array)))

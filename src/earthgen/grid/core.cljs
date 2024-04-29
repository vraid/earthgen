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

(defn distance [a b]
  (let
   [nth (fn [n]
          (square (- (js-array/get a n) (js-array/get b n))))]
    (Math/sqrt (+ (nth 0) (nth 1) (nth 2)))))

(defn create-edges [tile-vec corner-vertices]
  (let
   [tile-count (js-array/count tile-vec)
    arr (js-array/make (* 3 (- tile-count 2)) nil)]
    (loop [n 0
           id 0]
      (if (= id tile-count)
        arr
        (let
         [tile (js-array/get tile-vec id)
          {tiles :tiles
           corners :corners} tile
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
                      edge {:length (distance (corner-vertex k) (corner-vertex (mod (inc k) (js-array/count corners))))}
                      t (js-array/get tile-vec adj)]
                      (aset (:edges tile) k edge-id)
                      (aset (:edges t) (.indexOf (:tiles t) id) edge-id)
                      (aset arr edge-id edge)
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

(defn create-corners [tile-vec old-tiles]
  (let
   [old-tile-count (js-array/count old-tiles)
    arr (js-array/make (* 2 (- (js-array/count tile-vec) 2)) nil)
    tile-center (tile-center tile-vec)]
    (loop [n 0
           id 0]
      (if (= id old-tile-count)
        arr
        (let
         [tile (js-array/get old-tiles id)
          {center :center
           tiles :tiles} tile
          midpoint (midpoint center)
          n (loop [corner-id n
                   ls (seq (pairwise false tiles))]
              (if (empty? ls)
                corner-id
                (let
                 [[a b] (first ls)]
                  (if (and (< id a) (< id b))
                    (let
                     [corner {:tiles #js [id a b]
                              :vertex (midpoint (tile-center a) (tile-center b))}
                      set-in (fn [n adj]
                               (let
                                [t (js-array/get tile-vec n)]
                                 (aset (:corners t) (.indexOf (:tiles t) adj) corner-id)))]
                      (set-in id b)
                      (set-in a id)
                      (set-in b a)
                      (aset arr corner-id corner)
                      (recur (inc corner-id) (rest ls)))
                    (recur corner-id (rest ls))))))]
          (recur n (inc id)))))))

(defn tile-area [corner-vertices edge-lengths]
  (fn [{:keys [center corners edges]}]
    (let
     [count (js-array/count corners)
      distances (js-array/map (fn [n] (distance center (js-array/get corner-vertices n))) corners)
      dist (fn [n] (js-array/get distances (mod n count)))]
      (transduce (map (fn [n]
                        (trig/triangle-area
                         (js-array/get edge-lengths (js-array/get edges n))
                         (dist n)
                         (dist (inc n)))))
                 +
                 0.0
                 (range count)))))

(defn tile-with-area [area]
  (fn [a]
    (assoc a :area (area a))))

(defn corner-with-corners [tile-vec]
  (fn [id {:keys [tiles] :as a}]
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
   [tiles (js-array/concat old-tiles new-tiles)
    corners (create-corners tiles old-tiles)
    corner-vertices (js-array/map :vertex corners)
    edges (create-edges tiles corner-vertices)
    edge-lengths (js-array/map :length edges)
    final-tiles (js-array/map (tile-with-area (tile-area corner-vertices edge-lengths))
                              tiles)
    final-corners (js-array/map-indexed (corner-with-corners final-tiles)
                                        corners)]
    {:tiles final-tiles
     :corners final-corners
     :edges edges}))

(defn subdivide [grid]
  (let
   [tiles (:tiles grid)
    tile-count (js-array/count tiles)
    new-tile-id (partial + tile-count)
    corners (:corners grid)]
    (grid-from-tiles
     (js-array/map (fn [a]
                     (let
                      [corners (:corners a)
                       corner-count (js-array/count corners)]
                       {:center (:center a)
                        :tiles (js-array/map new-tile-id corners)
                        :corners (js-array/make corner-count -1)
                        :edges (js-array/make corner-count -1)}))
                   tiles)
     (js-array/map (fn [corner]
                     {:center (:vertex corner)
                      :tiles (let
                              [[b d f] (:tiles corner)
                               [a c e] (js-array/map new-tile-id (:corners corner))]
                               #js [a b c d e f])
                      :corners (js-array/make 6 -1)
                      :edges (js-array/make 6 -1)})
                   corners))))

(defn initial []
  (grid-from-tiles
   (apply array
          (mapv (fn [vertex tiles]
                  {:center vertex
                   :tiles (apply array tiles)
                   :corners (js-array/make 5 -1)
                   :edges (js-array/make 5 -1)})
                icosahedron/vertices
                icosahedron/indices))
   (array)))

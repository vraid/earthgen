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
   [arr (js-array/make (* 2 (- (js-array/count tile-vec) 2)) nil)
    tile-center (tile-center tile-vec)]
    (loop [n 0
           ls (seq old-tiles)]
      (if (empty? ls)
        arr
        (let
         [{:keys [id center tiles]} (first ls)
          midpoint (midpoint center)
          n (loop [n n
                   ls (seq (pairwise false tiles))]
              (if (empty? ls)
                n
                (let
                 [[a b] (first ls)]
                  (if (and (< id a) (< id b))
                    (let
                     [corner-id n
                      corner {:id n
                              :tiles #js [id a b]
                              :vertex (midpoint (tile-center a) (tile-center b))}
                      set-in (fn [n adj]
                               (let
                                [t (js-array/get tile-vec n)]
                                 (aset (:corners t) (.indexOf (:tiles t) adj) corner-id)))]
                      (set-in id b)
                      (set-in a id)
                      (set-in b a)
                      (aset arr n corner)
                      (recur (inc n) (rest ls)))
                    (recur n (rest ls))))))]
          (recur n (rest ls)))))))

(defn distance [a b]
  (let
   [nth (fn [n]
          (square (- (js-array/get a n) (js-array/get b n))))]
    (Math/sqrt (+ (nth 0) (nth 1) (nth 2)))))

(defn tile-area [tile corners]
  (let
   [center (:center tile)
    count (js-array/count corners)
    vertices (js-array/map :vertex corners)
    distances (js-array/map (fn [v] (distance center v)) vertices)
    vertex (fn [n] (js-array/get vertices (mod n count)))
    dist (fn [n] (js-array/get distances (mod n count)))]
    (transduce (map (fn [n]
                      (trig/triangle-area
                       (distance (vertex n) (vertex (inc n)))
                       (dist n)
                       (dist (inc n)))))
               +
               0.0
               (range count))))

(defn tile-with-area [corner-vec]
  (fn [a]
    (assoc a :area (tile-area a (js-array/map (fn [n]
                                                (js-array/get corner-vec n))
                                              (:corners a))))))

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
    final-tiles (js-array/map (tile-with-area corner-vec)
                              tile-vec)
    final-corners (js-array/map (corner-with-corners final-tiles) corner-vec)]
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
                     (let
                      [corners (:corners a)]
                       {:id (:id a)
                        :center (:center a)
                        :tiles (js-array/map new-tile-id corners)
                        :corners (js-array/make (js-array/count corners) -1)}))
                   tiles)
     (js-array/map (fn [corner]
                     {:id (new-tile-id (:id corner))
                      :center (:vertex corner)
                      :tiles (let
                              [[b d f] (:tiles corner)
                               [a c e] (js-array/map new-tile-id (:corners corner))]
                               #js [a b c d e f])
                      :corners (js-array/make 6 -1)})
                   corners))))

(defn initial []
  (grid-from-tiles
   (apply array
          (mapv (fn [id vertex tiles]
                  {:id id
                   :center vertex
                   :tiles (apply array tiles)
                   :corners (js-array/make 5 -1)})
                (range 12)
                icosahedron/vertices
                icosahedron/indices))
   (array)))

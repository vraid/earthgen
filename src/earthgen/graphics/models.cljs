(ns earthgen.graphics.models
  (:require [earthgen.interop.array :as js-array]
            [earthgen.math.quaternion :as quaternion]
            [earthgen.math.matrix :as matrix]
            [earthgen.grid.core :as grid]))

(defn make-buffers [size]
  {:vertices (js/Float32Array. (* 3 size))
   :colors (js/Float32Array. (* 4 size))})

(defn vertex-data [vertex-buffer color-buffer face-count]
  {:attribs
   {:position
    {:size 3
     :data vertex-buffer}
    :color
    {:size 4
     :data color-buffer}}
   :mode 4
   :num-vertices (* 3 face-count)
   :num-faces face-count})

(defn buffer-insert [buffer size data offset]
  (.set buffer data (* size offset)))

(defn element-insert [vertex-buffer color-buffer offset a]
  (let
   [faces (:faces a)
    colors (:colors a)
    count (js-array/count faces)]
    (doseq [n (range count)]
      (let
       [offset (* 3 (+ offset n))]
        (buffer-insert vertex-buffer 3 (aget faces n) offset)
        (buffer-insert color-buffer 4 (aget colors n) offset)))
    (+ offset count)))

(defn to-model [elements]
  (let
   [vertex-count (js-array/reduce (fn [sum a]
                                    (+ sum (:vertex-count a)))
                                  0
                                  elements)
    {vertex-buffer :vertices
     color-buffer :colors} (make-buffers vertex-count)
    count (js-array/count elements)
    insert (partial element-insert vertex-buffer color-buffer)
    _ (.fill vertex-buffer 0)
    _ (.fill color-buffer 0)
    face-count (loop [n 0
                      offset 0]
                 (if (= n count)
                   offset
                   (recur (inc n) (insert offset (js-array/get elements n)))))]
    (vertex-data vertex-buffer color-buffer face-count)))

(defn tiles-base [f]
  (fn [projection color planet]
    (let
     [rotate (matrix/vector-product (quaternion/to-matrix (:rotation planet)))
      corner-vertices (js-array/map (comp rotate :vertex)
                                    (:corners planet))
      [colors tile-color] (color planet)
      tile-colors (js-array/map (comp tile-color :id) (:tiles planet))]
      (to-model
       (js-array/map (fn [tile]
                       (let
                        [tile-center (rotate (:center tile))
                         proj (projection tile-center)
                         center (proj tile-center)
                         vertices (js-array/map (comp proj #(js-array/get corner-vertices %)) (:corners tile))
                         faces (grid/pairwise
                                center
                                vertices)]
                         (f colors tile-colors tile faces)))
                     (:tiles planet))))))

(def contoured-tiles
  (tiles-base (fn [colors tile-colors tile faces]
                (let
                 [face-count (js-array/count faces)
                  band (js-array/get tile-colors (:id tile))
                  contour? (js-array/map (fn [n] (not (= band (js-array/get tile-colors n))))
                                         (:tiles tile))
                  towards (fn [k u v]
                            (js-array/map (fn [a b]
                                            (+ (* k a) (* (- 1.0 k) b)))
                                          u v))
                  color (js-array/get colors band)
                  color-vec (js-array/concat color color color)
                  black #js [0 0 0 1]
                  black-vec (js-array/concat black black black)
                  corner (fn [vertices]
                           {:vertex-count 3
                            :vertices vertices
                            :colors black-vec})
                  pieces (js-array/reduce
                          js-array/concat
                          #js []
                          (js-array/build
                           face-count
                           (fn [n]
                             (let
                              [[c a b] (js-array/get faces n)
                               face (js-array/concat c a b)
                               [lc contour? rc] (js-array/map (fn [k]
                                                                (js-array/get contour? (mod (+ k n) face-count)))
                                                              #js [-1 0 1])]
                               (if contour?
                                 #js [{:vertex-count 6
                                       :vertices (js-array/concat
                                                  (js-array/concat
                                                   c
                                                   (towards 0.1 c a)
                                                   (towards 0.1 c b))
                                                  face)
                                       :colors (js-array/concat color-vec black-vec)}]
                                 (let
                                  [lcorner (if lc
                                             #js [(corner
                                                   (js-array/concat
                                                    a
                                                    (towards 0.075 b a)
                                                    (towards 0.1 c a)))]
                                             #js [])
                                   rcorner (if rc
                                             #js [(corner
                                                   (js-array/concat
                                                    b
                                                    (towards 0.1 c b)
                                                    (towards 0.075 a b)))]
                                             #js [])]
                                   (js-array/concat
                                    #js [{:vertex-count 3
                                          :vertices face
                                          :colors color-vec}]
                                    lcorner
                                    rcorner)))))))]
                  {:vertex-count (js-array/reduce + 0 (js-array/map :vertex-count pieces))
                   :faces (js-array/map :vertices pieces)
                   :colors (js-array/map :colors pieces)}))))

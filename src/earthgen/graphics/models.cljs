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
      [bands colors tile-color corner-color] (color planet)]
      (to-model
       (js-array/map (fn [tile]
                       (let
                        [tile-center (rotate (:center tile))
                         proj (projection tile-center)
                         center (proj tile-center)
                         faces (grid/pairwise-concat
                                center
                                (js-array/map (comp proj #(js-array/get corner-vertices %))
                                              (:corners tile)))]
                         (f bands colors tile-color corner-color tile faces)))
                     (:tiles planet))))))

(def solid-tiles
  (tiles-base (fn [_ colors tile-color _ tile faces]
                (let
                 [face-count (js-array/count faces)
                  [n _] (tile-color tile)
                  color (js-array/get colors n)
                  color-vec (js-array/concat color color color)]
                  {:vertex-count (* 3 face-count)
                   :faces faces
                   :colors (js-array/make face-count color-vec)}))))

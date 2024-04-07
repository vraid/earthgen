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

(defn element-insert [vertex-buffer color-buffer n a]
  (loop [n n
         faces (:faces a)
         colors (:colors a)]
    (if (empty? faces)
      n
      (do
        (buffer-insert vertex-buffer 3 (apply js-array/concat (first faces)) (* 3 n))
        (buffer-insert color-buffer 4 (apply js-array/concat (first colors)) (* 3 n))
        (recur (inc n) (rest faces) (rest colors))))))

(defn to-model [elements]
  (let
   [vertex-count (reduce + 0 (map :vertex-count elements))
    {vertex-buffer :vertices
     color-buffer :colors} (make-buffers vertex-count)
    insert (partial element-insert vertex-buffer color-buffer)
    _ (.fill vertex-buffer 0)
    _ (.fill color-buffer 0)
    face-count (loop [n 0
                      elements elements]
                 (if (empty? elements)
                   n
                   (recur (insert n (first elements))
                          (rest elements))))]
    (vertex-data vertex-buffer color-buffer face-count)))

(defn solid-tiles [projection color planet]
  (let
   [rotate (matrix/vector-product (quaternion/to-matrix (:rotation planet)))
    corners (mapv (fn [a]
                    (update a :vertex rotate))
                  (:corners planet))]
    (to-model
     (mapv (fn [tile]
             (let
              [tile-center (rotate (:center tile))
               proj (projection tile-center)
               center (proj tile-center)
               faces (grid/pairwise center (mapv (comp proj :vertex (partial nth corners))
                                                 (:corners tile)))
               face-count (count faces)
               tile-color ((color planet) tile)]
               {:vertex-count (* 3 face-count)
                :faces faces
                :colors (repeat face-count (repeat 3 tile-color))}))
           (:tiles planet)))))

(ns earthgen.math.vector)

(defn invalid? [a]
  (js/isNaN (reduce + 0 a)))

(defn integral [a]
  (mapv Math/round a))

(defn square [a]
  (* a a))

(defn squared-length [a]
  (transduce (map square) + 0 a))

(defn length [a]
  (Math/sqrt (squared-length a)))

(defn squared-distance [a b]
  (loop [sum 0.0
         as a
         bs b]
    (if (empty? as)
      sum
      (recur (+ sum (square (- (first as) (first bs)))) (rest as) (rest bs)))))

(defn distance [a b]
  (Math/sqrt (squared-distance a b)))

(defn scale-by [factor a]
  (mapv (partial * factor) a))

(defn scale-to [k a]
  (scale-by (/ k (length a)) a))

(defn normal [a]
  (scale-to 1 a))

(defn sum [a b]
  (mapv + a b))

(defn subtract [a b]
  (mapv - b a))

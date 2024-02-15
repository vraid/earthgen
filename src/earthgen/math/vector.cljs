(ns earthgen.math.vector)

(defn integral [a]
  (mapv Math/round a))

(defn square [a]
  (* a a))

(defn squared-length [a]
  (reduce + 0 (map (fn [n] (square n)) a)))

(defn length [a]
  (Math/sqrt (squared-length a)))

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

(ns earthgen.interop.array
  (:refer-clojure :exclude [get map reduce concat count take drop]))

(def get aget)

(defn map
  ([f a] (amap a n _ (f (aget a n))))
  ([f a b] (amap a n _ (f (aget a n) (aget b n)))))

(defn reduce [f init a]
  (areduce a n accum init (f accum (aget a n))))

(defn concat
  ([a b] (.concat a b))
  ([a b c] (.concat a b c)))

(defn count [a]
  (alength a))

(defn take [n arr]
  (.slice arr 0 n))

(defn drop [n arr]
  (.slice arr n (count arr)))

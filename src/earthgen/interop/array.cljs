(ns earthgen.interop.array
  (:refer-clojure :exclude [get map map-indexed reduce concat count first last take drop]))

(def get aget)

(defn access [arr]
  (fn [n]
    (get arr n)))

(defn map
  ([f a] (amap a n _ (f (get a n))))
  ([f a b] (amap a n _ (f (get a n) (get b n))))
  ([f a b c] (amap a n _ (f (get a n) (get b n) (get c n)))))

(defn map-indexed
  ([f a] (amap a n _ (f n (get a n))))
  ([f a b] (amap a n _ (f n (get a n) (get b n))))
  ([f a b c] (amap a n _ (f n (get a n) (get b n) (get c n)))))

(defn reduce [f init a]
  (areduce a n accum init (f accum (get a n))))

(defn concat
  ([a b] (.concat a b))
  ([a b c] (.concat a b c)))

(defn count [a]
  (alength a))

(defn first [a]
  (get a 0))

(defn last [a]
  (get a (dec (count a))))

(defn take [n arr]
  (.slice arr 0 n))

(defn drop [n arr]
  (.slice arr n (count arr)))

(defn make [count init]
  (.fill (array count) init))

(defn build [count f]
  (let
   [arr (make count nil)]
    (doseq [n (range count)]
      (aset arr n (f n)))
    arr))

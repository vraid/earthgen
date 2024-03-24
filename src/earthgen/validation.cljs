(ns earthgen.validation)

(defn validate-subdivisions [a]
  (let
   [subdivisions (parse-long a)]
    (if subdivisions (max 0 subdivisions) 0)))

(defn validate-timeout [a]
  (let
   [timeout (parse-long a)]
    (and timeout
         (< 0 timeout)
         timeout)))

(defn validate-terrain [a]
  (let
   [seed (:seed a)
    granularity (parse-long (:granularity a))
    irregularity (parse-double (:irregularity a))
    amplitude (parse-double (:amplitude a))
    sea-level (parse-double (:sea-level a))]
    {:seed seed
     :granularity (if granularity (max 0 granularity) 0)
     :irregularity (if irregularity (max 0 (min 1 irregularity)) 0)
     :amplitude (or amplitude 0)
     :sea-level (if sea-level sea-level 0)}))

(defn simple-terrain-str-values [a]
  {:seed (:seed a)
   :granularity (str (:granularity a))
   :irregularity (str (:irregularity a))
   :amplitude (str (:amplitude a))
   :sea-level (str (:sea-level a))})

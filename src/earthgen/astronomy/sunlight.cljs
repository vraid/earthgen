(ns earthgen.astronomy.sunlight)

(def half-pi (* 0.5 Math/PI))

(defn no-sunrise? [solar-declination latitude]
  (<= half-pi (Math/abs (- solar-declination latitude))))

(defn no-sunset? [solar-declination latitude]
  (let
   [solar-sign (Math/sign solar-declination)
    latitude-sign (Math/sign latitude)]
    (and (= 1 (* solar-sign latitude-sign))
         (>= (Math/abs latitude)
             (- half-pi (Math/abs solar-declination))))))

(defn potential-solar-radiation-integral [high low sunrise]
  (let
   [cos-sunrise (Math/cos sunrise)]
    (* (/ 1.0 Math/PI)
       (/ (+ (* (- high low)
                (Math/sin sunrise))
             (* sunrise
                (- low (* high cos-sunrise))))
          (- cos-sunrise 1)))))

(defn solar-high [solar-declination latitude]
  (Math/cos (- solar-declination latitude)))

(defn solar-low [solar-declination latitude]
  (Math/cos (- (Math/abs solar-declination)
               (- Math/PI (Math/abs latitude)))))

(defn sunrise-rotation [solar-declination latitude]
  (- (Math/acos (- (* (Math/tan solar-declination)
                      (Math/tan latitude))))))

(defn potential-solar-radiation-no-sunset [solar-declination latitude]
  (potential-solar-radiation-integral
   (solar-high solar-declination latitude)
   (solar-low solar-declination latitude)
   (- Math/PI)))

(defn potential-solar-radiation-default [solar-declination latitude]
  (potential-solar-radiation-integral
   (solar-high solar-declination latitude)
   0.0
   (sunrise-rotation solar-declination latitude)))

(defn potential-solar-radiation [solar-declination]
  (fn [latitude]
    (cond
      (no-sunrise? solar-declination latitude) 0.0
      (no-sunset? solar-declination latitude) (potential-solar-radiation-no-sunset solar-declination latitude)
      :else (potential-solar-radiation-default solar-declination latitude))))

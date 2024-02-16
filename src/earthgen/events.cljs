(ns earthgen.events
  (:require [re-frame.core :as re-frame]
            [earthgen.db :as db]))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::set-shader
 (fn [db [_ shader]]
   (assoc-in db [:graphics :shader] shader)))

(defn update-current-perspective [db]
  (assoc-in db [:graphics :perspective] (:perspective db)))

(re-frame/reg-event-fx
 ::tick
 (fn [cofx _]
   (let
    [db (:db cofx)
     time (:time-per-frame db)]
     {:db (update-current-perspective db)
      :fx [[:dispatch-later {:ms time :dispatch [::tick]}]]})))

(re-frame/reg-event-db
 ::mouse-down
 (fn [db [_ coord extra]]
   (let
    [register? (= 0 (:button extra))
     mouse-down (:mouse-down db)]
     (-> db
         (assoc :mouse-event [:down coord extra])
         (assoc :mouse-down
                (if (or mouse-down
                        (not register?))
                  mouse-down
                  (assoc extra
                         :coord coord
                         :model (:perspective db))))))))

(re-frame/reg-event-db
 ::mouse-up
 (fn [db [_ coord extra]]
   (let
    [mouse-down (:mouse-down db)
     end-mouse-down? (and mouse-down
                          (= (:button mouse-down)
                             (:button extra)))]
     (-> db
         (assoc :mouse-event [:up coord extra])
         (assoc :mouse-down (and (not end-mouse-down?)
                                 mouse-down))))))

(defn clamp [a b c]
  (max a (min b c)))

(re-frame/reg-event-db
 ::mouse-move
 (fn [db [_ coord]]
   (let
    [mouse-down (:mouse-down db)
     [_ prev] (:mouse-event db)
     rotation (and mouse-down
                   (let
                    [[x y] coord
                     [down-x down-y] (:coord mouse-down)
                     xdiff (- down-x x)
                     ydiff (- down-y y)
                     model (:rotation (:model mouse-down))
                     [_ _ width height] (:bounding-rect mouse-down)]
                     {:latitude (clamp (* -0.5 Math/PI)
                                       (* 0.5 Math/PI)
                                       (+ (:latitude model)
                                          (* ydiff 1.4 (/ Math/PI height))))
                      :longitude (+ (:longitude model)
                                    (* xdiff 1.4 (/ Math/PI width)))}))]
     (-> db
         (assoc :mouse-event [:move coord prev])
         (update-in [:perspective :rotation] (fn [a] (or rotation a)))))))

(ns earthgen.events
  (:require [re-frame.core :as re-frame]
            [earthgen.db :as db]
            [earthgen.math.vector :as vector]))

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
     mouse-down (get-in db [:input :mouse-down])]
     (-> db
         (assoc-in [:input :mouse-event] [:down coord extra])
         (assoc-in [:input :mouse-down]
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
    [mouse-down (get-in db [:input :mouse-down])
     end-mouse-down? (and mouse-down
                          (= (:button mouse-down)
                             (:button extra)))]
     (-> db
         (assoc-in [:input :mouse-event] [:up coord extra])
         (assoc-in [:input :mouse-down] (and (not end-mouse-down?)
                                             mouse-down))))))

(defn clamp [a b c]
  (max a (min b c)))

(defn update-rotation [model bounding-rect initial-coord coord]
  (let
   [{:keys [latitude longitude]} (:rotation model)
    [_ _ width height] bounding-rect
    [xdiff ydiff] (vector/subtract coord initial-coord)
    updated
    {:latitude (clamp (* -0.5 Math/PI)
                      (* 0.5 Math/PI)
                      (+ latitude
                         (* ydiff 0.8 (/ Math/PI height))))
     :longitude (+ longitude
                   (* xdiff 0.8 (/ Math/PI width)))}]
    (fn [db]
      (assoc-in db [:perspective :rotation] updated))))

(re-frame/reg-event-db
 ::mouse-move
 (fn [db [_ coord]]
   (let
    [mouse-down (get-in db [:input :mouse-down])
     [_ prev] (get-in db [:input :mouse-event])
     rotation (and mouse-down
                   (update-rotation (:model mouse-down)
                                    (:bounding-rect mouse-down)
                                    (:coord mouse-down)
                                    coord))
     rotation (or rotation identity)]
     (-> db
         (assoc-in [:input :mouse-event] [:move coord prev])
         rotation))))

(re-frame/reg-event-db
 ::touch-start
 (fn [db [_ touches changed extra]]
   (if (get-in db [:intput :mouse-down])
     db
     (let
      [bounding-rect (:bounding-rect extra)]
       (-> db
           (assoc-in [:input :touch-start] {:touches touches
                                            :bounding-rect bounding-rect
                                            :model (:perspective db)})
           (assoc-in [:input :touch-event] [:start touches changed]))))))

(re-frame/reg-event-db
 ::touch-end
 (fn [db [_ touches changed]]
   (assoc-in db [:input :touch-event] [:end touches changed])))

(re-frame/reg-event-db
 ::touch-move
 (fn [db [_ touches changed]]
   (let
    [touch-start (get-in db [:input :touch-start])
     start (:touches touch-start)
     rotation (and touch-start
                   (= 1 (count start))
                   (= 1 (count changed))
                   (update-rotation (:model touch-start)
                                    (:bounding-rect touch-start)
                                    (first (vals start))
                                    (first (vals changed))))
     rotation (or rotation identity)]
     (-> db
         (assoc-in [:input :touch-event] [:move touches changed])
         rotation))))

(re-frame/reg-event-db
 ::touch-cancel
 (fn [db [_ touches changed]]
   (-> db
       (assoc-in [:input :touch-event] [:cancel touches changed])
       (update-in [:input :touch-start] #(and % (seq touches))))))

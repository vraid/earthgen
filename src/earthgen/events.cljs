(ns earthgen.events
  (:require [re-frame.core :as re-frame]
            [earthgen.db :as db]
            [earthgen.perspective :as perspective]
            [earthgen.validation :as validation]
            [earthgen.generation.core :as generation]
            [earthgen.generation.generic :as generic]
            [earthgen.graphics.models :as models]
            [earthgen.graphics.map-modes :as map-modes]))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::set-shader
 (fn [db [_ shader]]
   (assoc-in db [:graphics :shader] shader)))

(re-frame/reg-event-db
 ::set-view
 (fn [db [_ view]]
   (assoc db :view view)))

(defn update-models [db]
  (assoc-in db
            [:graphics :models]
            [(models/solid-tiles
              (get-in db [:graphics :perspective :projection])
              map-modes/elevation
              (:planet db))]))

(defn update-current-perspective [db]
  (assoc-in db
            [:graphics :perspective]
            (get-in db [:perspectives (get-in db [:view :current-perspective])])))

(re-frame/reg-event-db
 ::set-perspective
 (fn [db [_ key]]
   (-> db
       (assoc-in [:view :current-perspective] key)
       update-current-perspective
       update-models)))

(defn generate [db subdivisions input]
  (try
    (let
     [grids (:grids db)
      subdivisions (validation/validate-subdivisions subdivisions)
      model (generic/from-input input)
      [_ planet] (generation/transform
                  grids
                  subdivisions
                  (generic/input-transforms model))]
      (-> db
          (assoc :model model)
          (assoc-in [:view :subdivisions] (str subdivisions))
          (assoc :planet planet)
          update-models))
    (catch js/Object _ (assoc db :model ""))))

(re-frame/reg-event-db
 ::generate
 (fn [db [_ subdivisions input]]
   (generate db subdivisions input)))

(re-frame/reg-event-db
 ::generate-simple
 (fn [db [_ subdivisions input]]
   (let
    [validated (validation/validate-terrain input)
     param (partial get-in validated)]
     (-> db
         (assoc-in [:view :simple-terrain] input)
         (generate subdivisions
                   (generic/terrain (param [:seed])
                                    (param [:sea-level])
                                    (generic/heightmap
                                     {:granularity (param [:granularity])
                                      :irregularity (param [:irregularity])
                                      :amplitude (param [:amplitude])})))))))

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
                            :model (perspective/current db))))))))

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

(re-frame/reg-event-db
 ::mouse-move
 (fn [db [_ coord]]
   (let
    [mouse-down (get-in db [:input :mouse-down])
     [_ prev] (get-in db [:input :mouse-event])
     rotation (and mouse-down
                   ((:update (perspective/current db))
                    (:model mouse-down)
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
                                            :model (perspective/current db)})
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
                   ((:update (perspective/current db))
                    (:model touch-start)
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

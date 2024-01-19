(ns earthgen.events
  (:require
   [re-frame.core :as re-frame]
   [earthgen.db :as db]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

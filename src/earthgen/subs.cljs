(ns earthgen.subs
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::graphics
 :graphics)

(re-frame/reg-sub
 ::view
 :view)

(re-frame/reg-sub
 ::model
 :model)

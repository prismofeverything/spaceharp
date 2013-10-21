(ns spaceharp.core
  (:use quil.core)
  (:require [bifocals.core :as bifocals]))

(defn setup
  []
  (smooth)
  (frame-rate 30))

(defn draw []
  (background 0)
  (bifocals/tick)
  (image (bifocals/depth-image) 0 0)
  (fill 200 50 100)
  (doseq [skeleton (vals @bifocals/skeletons)]
    (let [projection (bifocals/project-skeleton skeleton)
          [rightx righty rightz] (:right-hand projection)
          [leftx lefty leftz] (:left-hand projection)]
      (ellipse rightx righty 20 20)
      (ellipse leftx lefty 20 20))))

(defsketch kinect
  :title "SPACEHARP"
  :setup setup
  :draw draw
  :size [640 480])


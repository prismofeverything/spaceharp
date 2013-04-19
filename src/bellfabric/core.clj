(ns bellfabric.core
  (:use quil.core)
  (:require [bifocals.core :as bifocals]
            [overtone.core :as overtone]))

(defn setup
  []
  (smooth)
  (frame-rate 30))

(defn draw []
  (background 0)
  (bifocals/tick)
  (stroke 100)
  (doseq [skeleton (vals @bifocals/skeletons)]
    (let [right (:right-hand skeleton)
          left (:left-hand skeleton)]
      (ellipse (:x right) (:y right) 10 10)
      (ellipse (:x left) (:y left) 10 10))))

(defsketch kinect
  :title "BELLFABRIC"
  :setup setup
  :draw draw
  :size [640 480])


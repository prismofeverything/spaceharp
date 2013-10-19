(ns bellfabric.trail
  (:use [penumbra opengl])
  (:require [bifocals.core :as bifocals]
            [penumbra.app :as app]))

(defn reset
  [state]
  (merge 
   state
   {:fullscreen false}))

(defn find-largest-display-mode
  "run through all of the system display modes and find the one with the greatest area"
  []
  (let [res (fn [mode] (apply * (mode :resolution)))]
    (reduce #(if (> (res %1) (res %2)) %1 %2)
            {:resolution [0 0]}
            (app/display-modes))))

(defn set-largest-display-mode
  "find the largest display mode and set the resolution to that"
  []
  (let [largest (find-largest-display-mode)]
    (app/display-mode! largest)))

(defn init
  [state]
  (app/title! "BELL fabric")
  (app/vsync! true)
  ;; (set-largest-display-mode)
  (enable :depth-test)
  (enable :cull-face)
  (enable :lighting)
  (enable :light0)
  (enable :fog)
  (shade-model :flat)
  (reset state))

(defn reshape [[x y width height] state]
  (frustum-view 50 (/ (double width) height) 0.1 100)
  (load-identity)
  (translate 0 0 -3)
  (light 0
    :position [1 1 1 0])
  (fog
    :fog-mode :exp
    :fog-density 0.25
    :fog-start 0
    :fog-end 10
    :fog-color [0 0 0 0])
  (assoc state
    :width width
    :height height))

(defn key-press [key state]
  (cond
   (= key " ") (reset state)
   (= key :escape) (do 
                     (app/fullscreen! (not (state :fullscreen)))
                     (update-in state [:fullscreen] not))
   :else state))

(defn normalize-joint
  [[x y z]]
  [(* x 0.0015625) (* y 0.003125) (* z 0.0004)])

(defn normalize-skeleton
  [skeleton]
  (into 
   {}
   (for [[joint location] (seq skeleton)]
     [joint (normalize-joint location)])))

(defn update
  [[dt t] state]
  (bifocals/tick)
  (let [skeletons (map 
                   (fn [[user skeleton]]
                     (normalize-skeleton skeleton)) 
                   @bifocals/skeletons)]
    (assoc state 
      :test (* 0.5 (+ 1 (Math/sin (* t 1))))
      :skeletons skeletons)))

(defn display
  [[dt t] state]
  (if (> (count (:skeletons state)) 0)
    (do 
      (material 
       :front-and-back
       :ambient-and-diffuse 
       [1.0 (:test state) 0.25 1])
      (draw-polygon
       (apply vertex (-> state :skeletons first :right-hand))
       (apply vertex (-> state :skeletons first :left-hand))
       (vertex -0.5 -1.5 1)
       (vertex 0.5 -1.5 1)))
    (do 
      (material 
       :front-and-back
       :ambient-and-diffuse 
       [0.25 1.0 (:test state) 1])
      (draw-polygon
       (vertex 1 0 0)
       (vertex 1 0 0.5)
       (vertex 1 1 1)
       (vertex 0 1 0.5))))
  (app/repaint!))

(defn display-proxy
  [& args]
  (apply display args))

(defn start
  []
  (app/start 
   {:init init
    :reshape reshape
    :key-press key-press
    :update update
    :display display-proxy}
   {}))

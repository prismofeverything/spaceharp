(ns spaceharp.trail
  (:use [penumbra opengl])
  (:require [bifocals.core :as bifocals]
            [tonality.tonality :as tonality]
            [tonality.sound :as sound]
            [penumbra.app :as app]))

(defn reset
  [state]
  (merge 
   state
   {:fullscreen false
    :tonality (tonality/tonality tonality/pure-diatonic 100.0 0)
    :playing false
    :players {}}))

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
  (app/title! "SPACE harp")
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
  (translate 0 0 -4)
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
   (= key " ") (update-in state [:playing] not)
   (= key :escape) (do 
                     (app/fullscreen! (not (state :fullscreen)))
                     (update-in state [:fullscreen] not))
   :else state))

(defn normalize-joint
  [[x y z]]
  [(* x 0.001) (* y 0.002) (* z 0.0004)])

(defn make-bounds
  []
  {:left 100000.0
   :right -100000.0
   :bottom 100000.0
   :top -100000.0
   :front 100000.0
   :back -100000.0})

(def watched-limb?
  #{:right-hand :left-hand :right-foot :left-foot})

(defn shape-bounds
  [bounds limb [x y z]]
  (if (watched-limb? limb)
    (-> bounds
        (update-in [:left] #(Math/min % x))
        (update-in [:right] #(Math/max % x))
        (update-in [:bottom] #(Math/min % y))
        (update-in [:top] #(Math/max % y))
        (update-in [:front] #(Math/min % z))
        (update-in [:back] #(Math/max % z)))
    bounds))

(defn normalize-skeleton
  [skeleton]
  (reduce 
   (fn [[skeleton extremes] [limb point]]
     (let [normal (normalize-joint point)]
       [(assoc skeleton limb normal)
        (shape-bounds extremes limb normal)]))
   [{} (make-bounds)] (seq skeleton)))

(defn track-trails
  [trails skeleton limit]
  (reduce
   (fn [trails limb]
     (update-in trails [limb] #(take limit (conj % (get skeleton limb)))))
   trails watched-limb?))

(defn bin-interval
  [bottom interval index value]
  (int (Math/floor (/ (- (nth value index) bottom) interval))))

(defn trail-trigger?
  [trail bottom interval index threshhold]
  (if (> (count trail) threshhold)
    (let [binned (map (partial bin-interval bottom interval index) trail)
          batch (take threshhold binned)
          prototype (first batch)
          previous (nth binned threshhold)]
      (when (and (every? #(= % prototype) batch) (not= prototype previous))
        prototype))))

(defn play-harp
  [tonality tone amp]
  (sound/pretty-bell (tonality tone) 4.0 amp))

(defn update
  [[dt t] state]
  (bifocals/tick)
  (let [skeletons (into
                   {}
                   (for [[id skeleton] (seq @bifocals/skeletons)]
                     (let [[normal extremes] (normalize-skeleton skeleton)]
                       [id {:skeleton normal :extremes extremes}])))
        state (reduce 
               (fn [state id]
                 (update-in state [:trails id] track-trails (get-in skeletons [id :skeleton]) 10))
               state (keys skeletons))]
    (doseq [[id skeleton] (seq skeletons)]
      (doseq [[limb trail] (seq (select-keys (get-in state [:trails id]) [:right-hand :left-hand]))]
        ;; (println limb trail)
        (if-let [tone (trail-trigger? trail -1.5 0.1 1 7)]
          (do
            (println "TRIGGER" limb tone)
            (play-harp (:tonality state) tone 0.5)))))
    (assoc state
      :test (* 0.5 (+ 1 (Math/sin (* t 1))))
      :skeletons skeletons)))

(defn draw-skeleton
  [{:keys [skeleton extremes]}]
  (let [top (:top extremes)]
    ;; (println top)
    (material
     :front-and-back
     :ambient-and-diffuse [(- 1.0 (* 0.4 (+ 1.0 top)))
                           (+ 1.0 (* -1 (- top 0.2) (- top 0.2)))
                           (* (+ top 1.0) 0.4) 1.0])
    (draw-polygon
     (apply vertex (:right-hand skeleton))
     (apply vertex (:left-hand skeleton))
     (apply vertex (:left-foot skeleton))
     (apply vertex (:right-foot skeleton)))))

(defn display
  [[dt t] state]
  (if (> (count (:skeletons state)) 0)
    (doseq [[id skeleton] (seq (:skeletons state))]
      (draw-skeleton skeleton))
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

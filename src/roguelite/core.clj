(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import [java.awt.event KeyEvent]))


(def field-size [500 500])

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:player {:x 10 :y 10}})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  state)

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill 0 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [x (-> state :player :x)
        y (-> state :player :y)
        [sizex sizey] field-size
        nx (* 10 x)
        ny (* 10 y)]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circle.
      (q/ellipse nx ny 100 100))))

(defn event->direction [event]
  (case (:key event)
    (:w :up) [0 -1]
    (:s :down) [0 1]
    (:a :left) [-1 0]
    (:d :right) [1 0]
    [0 0]))


(defn key-pressed [state event]
  (let [[dx dy] (event->direction event)]
    (-> state
        (update-in [:player :x] #(+ % dx))
        (update-in [:player :y] #(+ % dy)))))


(q/defsketch roguelite
  :title "You spin my circle right round"
  :size field-size
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-pressed key-pressed
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.game :as game])
  (:import [java.awt.event KeyEvent]))


(def field-size [10 15])
(def screen-size [500 500])

;;; Input processing
(defn event->direction [event]
  (case (:key event)
    (:w :up) [0 -1]
    (:s :down) [0 1]
    (:a :left) [-1 0]
    (:d :right) [1 0]
    [0 0]))

(defn key-pressed [state event]
  (let [dir (event->direction event)
        world (:world state)]
    (update-in state [:player] #(game/move-gobject world % dir))))


;;;;; Drawing
(def tile-size 30)

(defn draw-gameobject [draw-region gobject]
  (q/with-fill (:color gobject)
    (let [nx (* tile-size (:posx gobject))
          ny (* tile-size (:posy gobject))]
      (q/text-char (:character gobject) nx ny))))

(defn draw-tile [tile]
  (if (:passable tile)
    (q/with-fill [0 0 0]
      (q/quad 0 0 0 tile-size tile-size tile-size tile-size 0))
    (q/with-fill (:color tile)
      (q/rect 1 1 (dec tile-size) (dec tile-size)))))

(defn draw-state [state]
  (q/background 0)
  (doseq [[x column] (:world state)]
    (doseq [[y tile] column]
      (q/with-translation [(* tile-size x) (* tile-size y)]
        (draw-tile tile))))

  (doseq [gobject (:objects state)]
    (draw-gameobject [] gobject))

  (q/fill 0 255 0)
  (q/with-translation [(/ tile-size 2) (/ tile-size 2)]
    (draw-gameobject [] (:player state))))


;;;;; Setup
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  (game/new-game field-size))

(defn update-state [state]
  state)

(q/defsketch roguelite
  :title "Roguelite"
  :size screen-size
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

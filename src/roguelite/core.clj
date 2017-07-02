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
    (case (:key event)
      (:r) (game/new-game field-size)  ;;; Restart
      (update-in state [:player] #(game/move-gobject world % dir)))))


;;;;; Drawing
(def tile-size 30)

(def tile-colors
  {:wall [20 20 20]
   :floor [10 10 10]
   :player [255 255 0]
   :zombie [127 255 127]})


(defn draw-gameobject [draw-region gobject]
  (q/with-fill ((:otype gobject) tile-colors)
    (let [nx (* tile-size (:posx gobject))
          ny (* tile-size (:posy gobject))]
      (q/text-char (:character gobject) nx ny))))


(defn draw-tile [tile]
  (if (:passable tile)
    (q/with-fill (:floor tile-colors)
      (q/rect 1 1 (dec tile-size) (dec tile-size)))
    (q/with-fill (:wall tile-colors)
      (q/rect 0 0 tile-size tile-size))))


(defn draw-state [state]
  (q/stroke-weight 0)
  (q/background 0)
  (doseq [[x column] (:world state)]
    (doseq [[y tile] column]
      (q/with-translation [(* tile-size x) (* tile-size y)]
        (draw-tile tile))))

  (q/with-translation [(/ tile-size 2) (/ tile-size 2)]
    (doseq [gobject (:objects state)]
      (draw-gameobject [] gobject))
    (q/with-fill (:player tile-colors)
      (draw-gameobject [] (:player state)))))


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

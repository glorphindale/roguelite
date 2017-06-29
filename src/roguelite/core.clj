(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.game :as game])
  (:import [java.awt.event KeyEvent]))


(def field-size [500 500])

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  (game/new-game))


(defn update-state [state]
  state)


(defn draw-gameobject [draw-region gobject]
  (q/with-fill (:color gobject)
    (let [nx (* 10 (:posx gobject))
          ny (* 10 (:posy gobject))]
      (q/text-char (:character gobject) nx ny))))


(defn draw-state [state]
  (q/background 0)
  (doseq [gobject (:objects state)]
    (draw-gameobject [] gobject))

  (q/fill 0 255 0)
  (draw-gameobject [] (:player state)))


(defn event->direction [event]
  (case (:key event)
    (:w :up) [0 -1]
    (:s :down) [0 1]
    (:a :left) [-1 0]
    (:d :right) [1 0]
    [0 0]))


(def tobject (game/->GameObject 10 10 \@ [0 0 0] :player))

(game/move-gobject tobject [10 10])


(defn key-pressed [state event]
  (let [dir (event->direction event)]
    (update-in state [:player] #(game/move-gobject % dir))))


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

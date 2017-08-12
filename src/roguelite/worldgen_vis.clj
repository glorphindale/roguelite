(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.worldgen :as wgen])
  (:import [java.awt.event KeyEvent]))

(defn key-pressed [state event]
  (case (:key event)
    (:r) (:tiles (wgen/simple-world [15 15] wgen/room-config))
    state))

(defn update-state [state]
  state)

(def tile-size 16)

(defn draw-tiles [tiles]
  (doseq [[x column] tiles]
    (doseq [[y tile] column]
      (q/with-translation [(* tile-size x) (* tile-size y)]
        (if (:passable tile)
          (q/text-char \. 4 -4)
          (q/text-char \# 0 0))))))

(defn draw-state [state]
  (q/stroke-weight 0)
  (q/background 0)

  (q/text (pr-str state) 20 20)
  (q/with-translation [100 100]
    (draw-tiles state)))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (:tiles (wgen/empty-world)))

(q/defsketch roguelite
  :title "Worldgen example"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features []
  :key-pressed key-pressed
  :middleware [m/fun-mode])

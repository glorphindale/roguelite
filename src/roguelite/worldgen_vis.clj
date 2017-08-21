(ns roguelite.worldgen-vis
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.worldgen :as wgen])
  (:import [java.awt.event KeyEvent]))

(def map-size [25 25])

(defn full-map []
  {:tiles (wgen/make-map map-size)
   :rooms []})

(defn add-room [state]
  (let [nroom (wgen/random-room map-size wgen/room-config)
        intersects? (filter #(wgen/intersects? % nroom) (:rooms state))]
    (if (empty? intersects?)
      (-> state
          (assoc-in [:new-room] nroom)
          (update-in [:rooms] conj nroom))
      (assoc-in state [:new-room] nroom)
      )))

(defn key-pressed [state event]
  (case (:key event)
    (:r) (full-map)
    (:n) (add-room state)
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

  (q/text (pr-str (:new-room state)) 20 20)
  (q/with-translation [100 100]
    (let [tiles (reduce #(wgen/carve-room %1 %2) (:tiles state) (:rooms state))]
      (draw-tiles tiles))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (full-map))

#_(q/defsketch roguelite
  :title "Worldgen example"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features []
  :key-pressed key-pressed
  :middleware [m/fun-mode])

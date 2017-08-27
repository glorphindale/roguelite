(ns roguelite.movement
  (:require [roguelite.utils :as utils]))

;; Movement

(defn gen-tile-coords [tiles]
  (for [[x col] tiles
        [y tile] col]
    [x y]))

(defn objects-at-pos [objects [tx ty]]
  "Returns a list of objects at position, indexed"
  (filter #(and (= tx (:posx (second %)))
                (= ty (:posy (second %))))
          (map-indexed vector objects)))

(defn get-impassable [objects]
  (letfn [(passable? [gobj] (get-in (second gobj) [:components :passable] false))]
    (filter (complement passable?) objects)))

(defn get-tile [world-map [mx my]]
  (get-in world-map [mx my]))

(defn new-position [gobject [dx dy]]
  [(+ (:posx gobject) dx) (+ (:posy gobject) dy)])

(defn offset-towards [[sx sy] [tx ty]]
  (let [dx (- tx sx)
        dy (- ty sy)
        distance (utils/dist [sx sy] [tx ty])]
    (if (> distance 0)
      (let [ox (Math/round (/ dx distance))
            oy (Math/round (/ dy distance))]
        [ox oy])
      [0 0])))

(defn move-possible? [state gobject dir]
  (let [new-pos (new-position gobject dir)
        passable (:passable (get-tile (:world state) new-pos))
        is-free (empty? (get-impassable (objects-at-pos (:objects state) new-pos)))]
    (and passable is-free)))

(defn move-gobject [state gobject dir]
  (let [[dx dy] dir
        [nx ny] (new-position gobject dir)
        player-occupied (and (= nx (-> state :player :posx)) (= ny (-> state :player :posy)))
        passable (move-possible? state gobject dir)]
    (if (and passable (not player-occupied)) 
      (-> gobject
          (assoc-in [:posx] nx)
          (assoc-in [:posy] ny))
      gobject)))



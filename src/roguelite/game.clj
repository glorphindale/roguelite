(ns roguelite.game
  (:require [roguelite.entities :as ent]
            [roguelite.worldgen :as wgen]))

;; Movement

(defn gen-tile-coords [tiles]
  (for [[x col] tiles
        [y tile] col]
    [x y]))

(defn objects-at-pos [objects [tx ty]]
  (filter #(and (= tx (:posx %)) (= ty (:posy %))) objects))

(defn get-tile [world-map [mx my]]
  (get-in world-map [mx my]))

(defn new-position [gobject [dx dy]]
  [(+ (:posx gobject) dx) (+ (:posy gobject) dy)])

(defn move-gobject [state gobject dir]
  (let [[dx dy] dir
        new-pos (new-position gobject dir)
        passable (:passable (get-tile (:world state) new-pos))
        is-free (empty? (objects-at-pos (:objects state) new-pos))]
    (if (and passable is-free)
      (-> gobject
          (update-in [:posx] #(+ % dx))
          (update-in [:posy] #(+ % dy)))
      gobject)))


;; Room gen
(defn empty-game []
  {:world (wgen/empty-world)
   :rooms [(wgen/make-room 1 1 3 3)]
   :player (ent/->GameObject 2 2 :player [])
   :objects []})

(defn new-game [map-size]
  (let [{:keys [rooms tiles]} (wgen/simple-world map-size wgen/room-config)
        [px py] (-> rooms first wgen/room-center)
        [zx zy] (-> rooms second wgen/room-center)]
    {:player (ent/->GameObject px py :player [])
     :objects (wgen/create-monsters (rest rooms))
     :world tiles
     :rooms rooms
     :state :start}))


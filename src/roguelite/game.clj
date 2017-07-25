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

(defn move-possible? [state gobject dir]
  (let [new-pos (new-position gobject dir)
        passable (:passable (get-tile (:world state) new-pos))
        is-free (empty? (objects-at-pos (:objects state) new-pos))]
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

;; High-level logic
(defn- apply-single-component [[gobject messages] component]
  (let [[ngobject message] (component gobject)]
    [ngobject (conj messages message)]))

(defn- apply-single-components [gobject]
  (reduce apply-single-component [gobject []] (-> gobject :components :self-components)))

(defn single-obj-components [state]
  (let [gobjs (:objects state)
        updated (map #(apply-single-components %1) gobjs)
        newgobjs (map first updated)
        messages (map second updated)]
    (-> state
        (assoc-in [:objects] newgobjs)
        (assoc-in [:messages] messages))))

(defn- apply-world-components [gobject state]
  (reduce #(%2 %1 state) gobject (-> gobject :components :world-components)))

(defn world-components [state]
  (let [gobjs (:objects state)
        newgobjs (map #(apply-world-components %1 state) gobjs)]
    (-> state
        (assoc-in [:objects] newgobjs))))

(defn one-step [state dir]
  (-> state
      (assoc-in [:state] :walking)
      (update-in [:player] #(move-gobject state % dir))
      (single-obj-components)  
      (world-components)))


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
     :messages []
     :state :start}))

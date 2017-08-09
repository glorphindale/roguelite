(ns roguelite.game
  (:require [roguelite.entities :as ent]
            [roguelite.movement :as move]
            [roguelite.worldgen :as wgen]))

;; High-level logic
(defn- apply-single-component [[gobject messages] component]
  (let [[ngobject message] (component gobject)]
    [ngobject (conj messages message)]))

(defn- apply-single-components [gobject]
  (reduce apply-single-component [gobject []] (-> gobject :components :self-components)))

(defn single-obj-components [state]
  (let [gobjs (:objects state)
        prev-messages (:messages state)
        updated (map #(apply-single-components %1) gobjs)
        newgobjs (map first updated)
        messages (map second updated)]
    (-> state
        (assoc-in [:objects] newgobjs)
        (assoc-in [:messages] (concat prev-messages messages)))))

(defn- apply-world-components [gobject state]
  (reduce #(%2 %1 state) gobject (-> gobject :components :world-components)))

(defn world-components [state]
  (let [gobjs (:objects state)
        newgobjs (map #(apply-world-components %1 state) gobjs)]
    (-> state
        (assoc-in [:objects] newgobjs))))

;; What to do?
(defn fight-components [state attacker defender]
  (let [component (-> attcker :components :fighter-components first)]
    (component attacker defender)))

(defn wait-step [state]
    (-> state
        (assoc-in [:state] :waiting)
        (assoc-in [:messages] []) ;; Clear messages
        (single-obj-components)
        (world-components)))

(defn one-step [state dir]
  (let [tobjects (move/objects-at-pos (:objects state) (move/new-position (:player state) dir))]
    (if (seq tobjects)
      (-> state
          (assoc-in [:state] :attacking)
          (assoc-in [:messages] [(str (ent/pretty-name (first tobjects)) " seems unamazed.")]))
      (-> state
          (assoc-in [:state] :walking)
          (assoc-in [:messages] []) ;; Clear messages
          (update-in [:player] #(move/move-gobject state % dir))
          (single-obj-components)
          (world-components)))))

;; Room gen
(defn new-game [map-size]
  (let [{:keys [rooms tiles]} (wgen/simple-world map-size wgen/room-config)]
    {:player (wgen/create-player (first rooms))
     :objects (wgen/create-monsters (rest rooms))
     :world tiles
     :rooms rooms
     :messages []
     :state :start}))

(defn simple-game []
  (let [{:keys [rooms tiles]} (wgen/empty-world)]
    {:player (wgen/create-player (first rooms))
     :objects (wgen/create-monsters (rest rooms))
     :world tiles
     :rooms rooms
     :messages []
     :state :start}))

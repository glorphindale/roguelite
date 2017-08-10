(ns roguelite.game
  (:require [roguelite.entities :as ent]
            [roguelite.components :as comps]
            [roguelite.movement :as move]
            [roguelite.worldgen :as wgen]))

;; High-level logic

(defn sound-component [state gobject-idx]
  (let [gobject (nth (:objects state) gobject-idx)]
    (if-let [sound (-> gobject :components :sound)]
      (let [[nobj msg] (comps/make-a-sound gobject)]
        (-> state
            (update-in [:messages] #(conj % msg))))
      state)))

(defn move-component [state gobject-idx]
  (letfn [(updater [gobj] (comps/roam state gobj))]
    (update-in state [:objects gobject-idx] updater)))

(defn process-gobject [state gobject-idx]
  (-> state
      (sound-component gobject-idx)
      (move-component gobject-idx)))

(defn process-gobjects [state]
  (let [idxs (-> state :objects count range)]
    (reduce #(process-gobject %1 %2) state idxs)))

(defn wait-step [state]
    (-> state
        (assoc-in [:state] :waiting)
        (assoc-in [:messages] []) ;; Clear messages
        (process-gobjects)
          ))

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
          (process-gobjects)
          ))))

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

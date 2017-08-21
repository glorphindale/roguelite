(ns roguelite.game
  (:require [roguelite.entities :as ent]
            [roguelite.components :as comps]
            [roguelite.movement :as move]
            [roguelite.worldgen :as wgen]))

;; High-level logic

(defn combat-round [attacker defender]
  (let [damage (max 0
                    (- (get-in attacker [:components :attacker :attack])
                       (get-in defender [:components :defender :defence])))
        nhp (- (get-in defender [:components :defender :hp]) damage)]
    (if (= damage 0)
      [attacker defender (str (ent/pretty-name defender) " is unamazed by strike.")]
      (if (> nhp 0)
        [attacker
         (assoc-in defender [:components :defender :hp] nhp)
         (str (ent/pretty-name attacker) " hits " (ent/pretty-name defender) " for " damage)]
        [attacker
         (ent/->GameObject (:posx defender) (:posy defender) :corpse {:passable true})
         (str (ent/pretty-name attacker) " slays " (ent/pretty-name defender) "!")]))))

(defn process-gobject [state gobject-idx]
  (-> state
      (comps/sound-component gobject-idx)
      (comps/move-component gobject-idx combat-round)))

(defn process-gobjects [state]
  (let [idxs (-> state :objects count range)]
    (reduce #(process-gobject %1 %2) state idxs)))

(defn objs-to-attack [gobjects target]
  (let [objs-at-pos (move/objects-at-pos gobjects target)]
    (filter #(get-in (second %) [:components :defender]) objs-at-pos)))

;;; Player actions
(defn attack [state gobject-idx]
  (let [attacker (:player state)
        defender (get-in state [:objects gobject-idx])
        [nattacker ndefender message] (combat-round attacker defender)]
    (-> state
        (assoc-in [:player] nattacker)
        (assoc-in [:objects gobject-idx] ndefender)
        (update-in [:messages] conj message))))

(defn wait-step [state]
    (-> state
        (assoc-in [:state] :waiting)
        (assoc-in [:messages] []) ;; Clear messages
        (process-gobjects)))

(defn one-step [state dir]
  (let [tobjects (objs-to-attack (:objects state)
                                 (move/new-position (:player state) dir))]
    (if (seq tobjects)
      (-> state
          (assoc-in [:state] :attacking)
          (assoc-in [:messages] [])
          (attack (ffirst tobjects))
          (process-gobjects))
      (-> state
          (assoc-in [:state] :walking)
          (assoc-in [:messages] []) ;; Clear messages
          (update-in [:player] #(move/move-gobject state % dir))
          (process-gobjects)
          ))))

;; Game gen
(defn new-game [map-size]
  (let [{:keys [rooms tiles]} (wgen/simple-world map-size wgen/room-config)]
    {:player (wgen/place-player (first rooms))
     :objects (wgen/create-monsters (rest rooms))
     :world tiles
     :rooms rooms
     :messages []
     :state :start}))

(defn simple-game []
  (let [{:keys [rooms tiles]} (wgen/empty-world)]
    {:player (wgen/place-player (first rooms))
     :objects (wgen/create-monsters (rest rooms))
     :world tiles
     :rooms rooms
     :messages []
     :state :start}))

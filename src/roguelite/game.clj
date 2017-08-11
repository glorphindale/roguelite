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
      [attacker defender (str (ent/pretty-name defender) " seems unamazed.")]
      (if (> nhp 0)
        [attacker
         (assoc-in defender [:components :defender :hp] nhp)
         (str (ent/pretty-name attacker) " hits " (ent/pretty-name defender) " for " damage)]
        [attacker
         nil
         (str (ent/pretty-name attacker) " slays " (ent/pretty-name defender) "!")]))))

(defn sound-component [state gobject-idx]
  (let [gobject (nth (:objects state) gobject-idx)]
    (if-let [sound (-> gobject :components :sound)]
      (let [[nobj msg] (comps/make-a-sound gobject)]
        (-> state
            (update-in [:messages] #(conj % msg))))
      state)))

(defn roam [state gobject-idx]
  (letfn [(updater [gobj] (comps/roam state gobj))]
    (update-in state [:objects gobject-idx] updater)))

(defn hunt [state gobject-idx]
  state
  )

(defn attack-nearby [state gobject-idx]
  (let [px (get-in state [:player :posx])
        py (get-in state [:player :posy])
        mx (get-in state [:objects gobject-idx :posx])
        my (get-in state [:objects gobject-idx :posy])]
    (if (comps/is-player-nearby? [px py] [mx my])
      (let [attacker (get-in state [:objects gobject-idx])
            defender (get-in state [:player])
            [nattacker ndefender message] (combat-round attacker defender)] 
        (if ndefender
          (-> state
              (assoc-in [:player] ndefender)
              (update-in [:messages] conj message))  
          (-> state
              (update-in [:messages] conj message)   
              (assoc-in [:state] :gameover))))
      (letfn [(updater [gobj] (comps/roam state gobj))]
        (update-in state [:objects gobject-idx] updater)))
    ))

(defn move-component [state gobject-idx]
  (let [gobject (get-in state [:objects gobject-idx])
        movement (get-in gobject [:components :movement])]
    (if (= :roam movement)
      (roam state gobject-idx)
      (if (= :attack-nearby movement)
        (attack-nearby state gobject-idx)
        state
        ))))

(defn process-gobject [state gobject-idx]
  (-> state
      (sound-component gobject-idx)
      (move-component gobject-idx)))

(defn process-gobjects [state]
  (let [idxs (-> state :objects count range)]
    (reduce #(process-gobject %1 %2) state idxs)))

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
  (let [tobjects (move/objects-at-pos (:objects state) (move/new-position (:player state) dir))]
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

;; Room gen
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

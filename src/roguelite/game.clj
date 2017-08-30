(ns roguelite.game
  (:require [roguelite.entities :as ent]
            [roguelite.components :as comps]
            [roguelite.fov :as fov]
            [roguelite.movement :as move]
            [roguelite.worldgen :as wgen]))

;; High-level logic

(defn apply-damage [attacker defender damage]
  (let [nhp (- (get-in defender [:components :defender :hp]) damage)]
    (if (> nhp 0)
      [(assoc-in defender [:components :defender :hp] nhp)
       (str (ent/pretty-name attacker) " hits " (ent/pretty-name defender) " for " damage)]
      [(ent/->GameObject (:posx defender) (:posy defender) :corpse {:passable true})
       (str (ent/pretty-name attacker) " slays " (ent/pretty-name defender) "!")]))) 

(defn combat-round [attacker defender]
  (let [damage (max 0
                    (- (get-in attacker [:components :attacker :attack])
                       (get-in defender [:components :defender :defence])))]
    (if (= damage 0)
      [attacker defender (str (ent/pretty-name defender) " is unamazed by strike.")]
      (let [[ndefender message] (apply-damage attacker defender damage)]
        [attacker ndefender message]))))


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
        (ent/+msg message))))

(defn wait-step [state]
    (-> state
        (assoc-in [:state] :waiting)
        (process-gobjects)))

(defn one-step [state dir]
  (let [tobjects (objs-to-attack (:objects state)
                                 (move/new-position (:player state) dir))]
    (if (seq tobjects)
      (-> state
          (assoc-in [:state] :attacking)
          (attack (ffirst tobjects))
          (process-gobjects))
      (-> state
          (assoc-in [:state] :walking)
          (update-in [:player] #(move/move-gobject state % dir))
          (process-gobjects)
          ))))

(defn- remove-nth [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn use-attack-potion [state idx]
  (-> state
      (ent/+msg "You drink, you strong.")
      (update-in [:player :components :attacker :attack] inc)))

(defn use-health-potion [state idx]
  (-> state
      (ent/+msg "You chug a health potion.")
      (update-in [:player :components :defender :hp] #(min (get-in state [:player :components :defender :max-hp]) (+ % 3)))))

(defn use-defence-potion [state idx]
  (-> state
      (ent/+msg "Barkskin potion tastes bad.")
      (update-in [:player :components :defender :defence] inc)))

(defn- extract-pos [[_ gobject]]
  [(:posx gobject) (:posy gobject)])

(defn use-lightning-scroll [state idx]
  (let [monsters (filter #(get-in (second %) [:components :defender] false)
                         (map-indexed vector (:objects state)))
        visible-monsters (filter #(some #{(extract-pos %)} (:visibility state)) monsters)
        target (first visible-monsters)]
    (if target
      (let [[gobj-idx defender] target
            [ndefender message] (apply-damage (:player state) defender 5)]  ;; HARDCODE FOR DAMAGE
        (-> state
            (ent/+msg (str "Lightning strikes the " (ent/pretty-name defender)))
            (ent/+msg message)
            (assoc-in [:objects gobj-idx] ndefender)))
      (-> state
          (ent/+msg "No targets nearby")))))

(defn pacify [gobjects]
  (letfn [(pacify-single [gobject] (if (get-in gobject [:components :movement])
                                     (assoc-in gobject [:components :movement] :roam)
                                     gobject))]
    (vec (map pacify-single gobjects))))

(defn rage [gobjects]
  (letfn [(rage-single [gobject] (if (get-in gobject [:components :movement])
                                   (assoc-in gobject [:components :movement] :hunt)
                                   gobject))]
    (vec (map rage-single gobjects))))

(defn use-pacify-scroll [state idx]
  (-> state
      (ent/+msg "Suddenly monsters seem less angry")
      (update-in [:objects] pacify)))

(defn use-rage-scroll [state idx]
  (-> state
      (ent/+msg "Suddenly monsters seem very angry")
      (update-in [:objects] rage)))

(defn use-scroll [state item idx]
  (try
    (let [effect (get-in item [:effect])]
      (case effect
        (:lightning) (use-lightning-scroll state idx)
        (:aggro) (use-rage-scroll state idx)
        (:pacify) (use-pacify-scroll state idx)
        (ent/+msg state (str "Effect " effect))))
    (catch Exception e (ent/+msg state e))))

(defn use-item [state key-pressed]
  (try
    (let [idx (Integer/parseInt (name key-pressed)) 
          item (get-in state [:player :components :inventory idx])
          changed-state (case (:itype item)
                          (:health-potion) (use-health-potion state idx)
                          (:attack-potion) (use-attack-potion state idx)
                          (:defence-potion) (use-defence-potion state idx)
                          (:scroll) (use-scroll state item idx)
                          (ent/+msg state (str "You break " (:itype item) " when using it." )))]
      (update-in changed-state [:player :components :inventory] remove-nth idx))
    (catch IndexOutOfBoundsException e (ent/+msg state "No such item"))  
    (catch NumberFormatException e (ent/+msg state "No such item"))))

(defn pickup [state]
  (let [px (-> state :player :posx)
        py (-> state :player :posy)
        objs (move/objects-at-pos (:objects state) [px py])
        items (filter #(= :item (-> % second :otype)) objs)]
    (if (seq items)
      (let [item-idx (first (map first items))
            item (first (map second items))
            item-to-add (-> item :components :item-props)]
        (-> state
            (update-in [:objects] remove-nth item-idx)
            (ent/+msg (str "You pickup a " (comps/itype->txt (:itype item-to-add))))
            (update-in [:player :components :inventory] conj item-to-add)))
      (ent/+msg state "Nothing to pickup"))))

(defn refresh-visibility [state]
  (if (:no-fog state)
    (-> state
        (assoc-in [:visibility] (move/gen-tile-coords (:world state)))
        (update-in [:world] #(fov/update-discovered (:visibility state) %)))
    (-> state
        (assoc-in [:visibility] (fov/get-visible-tiles
                                  [(get-in state [:player :posx]) (get-in state [:player :posy])]
                                  (:world state)))
        (update-in [:world] #(fov/update-discovered (:visibility state) %)))))

;; Game gen
(defn new-game [map-size]
  (let [{:keys [rooms tiles]} (wgen/simple-world map-size wgen/room-config)
        world {:player (wgen/place-player (first rooms))
               :objects (vec (concat (wgen/create-monsters (rest rooms)) (wgen/place-items rooms)))
               :world tiles
               :rooms rooms
               :messages []
               :state :start}]
    (refresh-visibility world)))

(defn simple-game []
  (let [{:keys [rooms tiles]} (wgen/empty-world)
        world {:player (wgen/place-player (first rooms))
               :objects (vec (concat (wgen/create-monsters (rest rooms)) (wgen/place-items rooms)))
               :world tiles
               :rooms rooms
               :messages []
               :state :start}]
    (refresh-visibility world)))

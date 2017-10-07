(ns roguelite.components
  (:require [clojure.string :as s]
            [roguelite.entities :as ent]
            [roguelite.movement :as move]))

;;; Available components:
;;;   :attacker {:attack 5}
;;;   :defender {:defence 2 :max-hp 2 :hp 3}
;;;   :passable true/false
;;;   :sound
;;;   :movement :roam/:attack-nearby/:hunt

(defn make-a-sound [gobject sound]
  (if (> (rand-int 16) 14)
    [gobject (str (ent/pretty-name gobject) " " sound "!")]
    [gobject nil]))

(defn nearby-cells [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]]
    [(+ x dx) (+ y dy)]))

(defn is-player-nearby? [ppos mpos]
  (seq (filter #(= %1 mpos) (nearby-cells ppos))))

(defn describe-defender [gobject]
  (when-let [defender (get-in gobject [:components :defender])]
    (let [ratio (/ (:hp defender) (:max-hp defender))
          defence (get-in gobject [:components :defender :defence])]
      (cond
       (< ratio 0.3) "It is severly injured."
       (< ratio 0.7) "It is injured."
       (< ratio 1) "It is slightly injured."
       (>= ratio 1) "It is uninjured."))))

(defn itype->txt [itype]
  (case itype
    (:health-potion) "Health potion"
    (:attack-potion) "Rage potion"
    (:defence-potion) "Barkskin potion"
    (:weapon) "Dull dagger"
    (:scroll) "Scroll of "
    (name itype)))

(defn describe-item [item]
  (let [itype (get-in item [:itype] :junk)]
    (case itype
      (:armor) (str (ent/pretty-value (:variant item)) " +" (:defence item)) 
      (:weapon) (str (ent/pretty-value (:variant item)) " +" (:attack item))
      (str (itype->txt itype) (name (get-in item [:effect] ""))))))

(defn describe-obj [gobject]
  (if (get-in gobject [:components :defender])
    (str "You see: " (ent/pretty-name gobject) "\n" (describe-defender gobject))
    (if (get-in gobject [:components :item-props])
      (str "You see: " (-> gobject :components :item-props describe-item))
      (str "You see: " (ent/pretty-name gobject)))))

(defn roam [state gobject-idx]
  (let [direction (rand-nth [[1 1] [1 -1] [-1 1] [-1 -1] [0 1] [1 0] [0 -1] [-1 0]])]
    (update-in state [:objects gobject-idx] #(move/move-gobject state % direction))))

(defn monster-attack [state combat-func monster player]
  (let [[nattacker ndefender message] (combat-func monster player)] 
    (if (= (:otype ndefender) :corpse)
      (-> state
          (ent/+msg message)   
          (assoc-in [:state] :gameover))  
      (-> state
          (assoc-in [:player] ndefender)
          (ent/+msg message)))))

(defn attack-nearby [state gobject-idx combat-func]
  (let [player (:player state)
        px (:posx player) py (:posy player)
        monster (get-in state [:objects gobject-idx])
        mx (:posx monster) my (:posy monster)]
    (if (is-player-nearby? [px py] [mx my])
      (monster-attack state combat-func monster player)
      (roam state gobject-idx))))

(defn hunt [state gobject-idx combat-func]
  " Would not work nicely when :use-fog is not active "
  (let [player (:player state)
        px (:posx player) py (:posy player)
        monster (get-in state [:objects gobject-idx])
        mx (:posx monster) my (:posy monster)]
    (if (some #{[mx my]} (get-in state [:visibility]))
      (if (is-player-nearby? [px py] [mx my])
        (monster-attack state combat-func monster player)
        (let [direction (move/offset-towards [mx my] [px py])]
          (update-in state [:objects gobject-idx] #(move/move-gobject state % direction))))
      (roam state gobject-idx))))

(defn sound-component [state gobject-idx]
  (let [gobject (nth (:objects state) gobject-idx)]
    (if-let [sound (-> gobject :components :sound)]
      (let [[nobj msg] (make-a-sound gobject sound)]
        (-> state
            (ent/+msg msg)))
      state)))

(defn move-component [state gobject-idx combat-func]
  (let [gobject (get-in state [:objects gobject-idx])
        movement (get-in gobject [:components :movement])]
    (case movement
      (:roam) (roam state gobject-idx)
      (:hunt) (hunt state gobject-idx combat-func)
      (:attack-nearby) (attack-nearby state gobject-idx combat-func)
      state)
    ))

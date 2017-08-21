(ns roguelite.components
  (:require [clojure.string :as s]
            [roguelite.entities :as ent]
            [roguelite.movement :as move]))

(defn make-a-sound [gobject]
  (let [sound (rand-nth ["howls" "growls" "roars"])]
    (if (> (rand-int 5) 3)
      [gobject (str (ent/pretty-name gobject) " " sound "!")]
      [gobject nil])))

(defn nearby-cells [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]]
    [(+ x dx) (+ y dy)]))

(defn is-player-nearby? [ppos mpos]
  (seq (filter #(= %1 mpos) (nearby-cells ppos))))

(defn describe-defender [gobject]
  (when-let [defender (get-in gobject [:components :defender])]
    (let [ratio (/ (:hp defender) (:max-hp defender))]
      (cond
       (< ratio 0.3) "It is severly injured."
       (< ratio 0.7) "It is injured."
       (< ratio 1) "It is slightly injured."
       (>= ratio 1) "It is uninjured."))))

(defn roam [state gobject-idx]
  (let [direction (rand-nth [[1 1] [1 -1] [-1 1] [-1 -1] [0 1] [1 0] [0 -1] [-1 0]])]
    (update-in state [:objects gobject-idx] #(move/move-gobject state % direction))))

(defn hunt [state gobject-idx]
  state)

(defn attack-nearby [state gobject-idx combat-func]
  (let [px (get-in state [:player :posx])
        py (get-in state [:player :posy])
        mx (get-in state [:objects gobject-idx :posx])
        my (get-in state [:objects gobject-idx :posy])]
    (if (is-player-nearby? [px py] [mx my])
      (let [attacker (get-in state [:objects gobject-idx])
            defender (get-in state [:player])
            [nattacker ndefender message] (combat-func attacker defender)] 
        (if (= (:otype ndefender) :corpse)
          (-> state
              (update-in [:messages] conj message)   
              (assoc-in [:state] :gameover))  
          (-> state
              (assoc-in [:player] ndefender)
              (update-in [:messages] conj message))))
      (roam state gobject-idx))))

;;; Available components
(defn sound-component [state gobject-idx]
  (let [gobject (nth (:objects state) gobject-idx)]
    (if-let [sound (-> gobject :components :sound)]
      (let [[nobj msg] (make-a-sound gobject)]
        (-> state
            (update-in [:messages] #(conj % msg))))
      state)))

(defn move-component [state gobject-idx combat-func]
  (let [gobject (get-in state [:objects gobject-idx])
        movement (get-in gobject [:components :movement])]
    (case movement
      (:roam) (roam state gobject-idx)
      (:attack-nearby) (attack-nearby state gobject-idx combat-func)
      state)
    ))

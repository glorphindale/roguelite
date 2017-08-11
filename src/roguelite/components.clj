(ns roguelite.components
  (:require [clojure.string :as s]
            [roguelite.entities :as ent]
            [roguelite.movement :as move]))

(defn make-a-sound [gobject]
  (let [sound (rand-nth ["howls" "growls" "roars"])]
    (if (> (rand-int 5) 3)
      [gobject (str (ent/pretty-name gobject) " " sound "!")]
      [gobject nil])))

(defn roam [state gobject]
  (if (= :roam (-> gobject :components :movement))
    (let [direction (rand-nth [[0 1] [1 0] [0 -1] [-1 0]])]
      (move/move-gobject state gobject direction))
    gobject))

(defn nearby-cells [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]]
    [(+ x dx) (+ y dy)]))

(defn is-player-nearby? [ppos mpos]
  (seq (filter #(= %1 mpos) (nearby-cells ppos))))

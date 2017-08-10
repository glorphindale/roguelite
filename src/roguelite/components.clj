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


(defn fighter []
  (fn [attacker defender]
    [attacker nil]))

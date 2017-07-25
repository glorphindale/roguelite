(ns roguelite.components
  (:require [clojure.string :as s]
            [roguelite.game :as game]))

;; Limitation: every object must have at least one of every component type, or it will be filtered out
(defn howler []
  (fn [gobject]
    (let [sound (rand-nth ["howls" "growls" "roars"])]
      (if (> (rand-int 5) 3)
        [gobject (str (-> gobject :otype name s/capitalize) " " sound "!")]
        [gobject nil]))))

(defn roamer []
  (fn [gobject state]
    (let [direction (rand-nth [[0 1] [1 0] [0 -1] [-1 0]])]
      (game/move-gobject state gobject direction)))) 

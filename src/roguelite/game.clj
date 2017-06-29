(ns roguelite.game)

(defrecord GameObject [posx posy character color otype])

(defn new-game[]
  {:player (->GameObject 10 10 \@ [255 255 0] :player)
   :objects [(->GameObject 5 5 \Z [127 255 127] :zombie)]})

(defn move-gobject [gobject dir]
  (println "!" gobject dir)
  (let [[dx dy] dir]

    (-> gobject
        (update-in [:posx] #(+ % dx))
        (update-in [:posy] #(+ % dy)))))

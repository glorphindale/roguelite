(ns roguelite.game)

(defrecord GameObject [posx posy character otype])
(defrecord Tile [passable blocks-sight])

(defn make-map [[mx my]]
  (letfn [(random-tile []
            (->Tile (rand-nth [true true true false]) false))
          (random-row []
            (zipmap (range 0 my) (repeatedly random-tile)))]
    (zipmap (range 0 mx) (repeatedly random-row))))

(defn get-tile [world-map [mx my]]
  (get-in world-map [mx my]))

(defn new-position [gobject [dx dy]]
  [(+ (:posx gobject) dx) (+ (:posy gobject) dy)])

(defn move-gobject [world-map gobject dir]
  (let [[dx dy] dir
        new-pos (new-position gobject dir)]
    (if (:passable (get-tile world-map new-pos))
      (-> gobject
          (update-in [:posx] #(+ % dx))
          (update-in [:posy] #(+ % dy)))
      gobject)))

(defn new-game [map-size]
  {:player (->GameObject 0 0 \@ :player)
   :objects [(->GameObject 5 5 \Z :zombie)]
   :world (make-map map-size)})

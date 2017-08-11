(ns roguelite.movement)

;; Movement

(defn gen-tile-coords [tiles]
  (for [[x col] tiles
        [y tile] col]
    [x y]))

(defn objects-at-pos [objects [tx ty]]
  (filter #(and (= tx (:posx (second %))) (= ty (:posy (second %))))
          (map-indexed vector objects)))

(defn get-tile [world-map [mx my]]
  (get-in world-map [mx my]))

(defn new-position [gobject [dx dy]]
  [(+ (:posx gobject) dx) (+ (:posy gobject) dy)])

(defn move-possible? [state gobject dir]
  (let [new-pos (new-position gobject dir)
        passable (:passable (get-tile (:world state) new-pos))
        is-free (empty? (objects-at-pos (:objects state) new-pos))]
    (and passable is-free)))

(defn move-gobject [state gobject dir]
  (let [[dx dy] dir
        [nx ny] (new-position gobject dir)
        player-occupied (and (= nx (-> state :player :posx)) (= ny (-> state :player :posy)))
        passable (move-possible? state gobject dir)]
    (if (and passable (not player-occupied)) 
      (-> gobject
          (assoc-in [:posx] nx)
          (assoc-in [:posy] ny))
      gobject)))



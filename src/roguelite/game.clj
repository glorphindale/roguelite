(ns roguelite.game)

(defrecord GameObject [posx posy character otype])
(defrecord Tile [passable blocks-sight])

(defn make-map [[mx my]]
  (letfn [(random-tile []
            (map->Tile {:passable false :blocks-sight true}))
          (random-row []
            (zipmap (range 0 my) (repeatedly random-tile)))]
    (zipmap (range 0 mx) (repeatedly random-row))))


(defn carve-tile [world [sx sy]]
  (-> world
      (assoc-in [sx sy :passable] true)
      (assoc-in [sx sy :blocks-sight] false) ))

(defn all-tiles [room]
  (let [sx (:sx room)
        sy (:sy room)]
    (for [x (range sx (+ sx (:width room)))
          y (range sy (+ sy (:height room)))]
      [x y])))

(defn carve-room [world room]
   (reduce #(carve-tile %1 %2)
           world
           (all-tiles room)))

(defn carve-h-tunnel [world x1 x2 y]
  (let [start (min x1 x2)
        end (max x1 x2)]
    (carve-room world {:sx start :sy y :width (- end start) :height 1})))

(defn carve-v-tunnel [world x y1 y2]
  (let [start (min y1 y2)
        end (max y1 y2)]
    (carve-room world {:sx x :sy start :width 1 :height (- end start)})))

;; ---------------------
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

;; ---------------------
(defn test-world [map-size]
  (-> (make-map map-size)
      (carve-room {:sx 4 :sy 4 :width 6 :height 10})
      (carve-room {:sx 0 :sy 4 :width 2 :height 5})
      (carve-room {:sx 0 :sy 0 :width 10 :height 2})
      (carve-h-tunnel 2 5 5)
      (carve-v-tunnel 6 2 5)
      ))

(defn new-game [map-size]
  {:player (->GameObject 7 7 \@ :player)
   :objects [(->GameObject 5 5 \Z :zombie)]
   :world (test-world map-size)})

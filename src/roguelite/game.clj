(ns roguelite.game)

(defrecord GameObject [posx posy character otype])
(defrecord Tile [passable blocks-sight])
(defrecord Room [x1 y1 x2 y2])

(defn make-room [x y width height]
  (->Room x y (+ x width) (+ y height)))

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

(defn- all-tiles [{:keys [x1 x2 y1 y2]}]
  (for [x (range x1 x2) y (range y1 y2)]
    [x y]))

(defn carve-room [world room]
   (reduce #(carve-tile %1 %2)
           world
           (all-tiles room)))

(defn carve-h-tunnel [world x1 x2 y]
  (let [start (min x1 x2)
        end (max x1 x2)]
    (carve-room world (make-room start y (- end start) 1))))

(defn carve-v-tunnel [world x y1 y2]
  (let [start (min y1 y2)
        end (max y1 y2)]
    (carve-room world (make-room x start 1 (- end start)))))

(defn room-center [room]
  [(int (/ (+ (:x1 room) (:x2 room)) 2)) (int (/ (+ (:y1 room) (:y2 room)) 2))])

(defn intersects? [room1 room2]
  (let [{x11 :x1 x21 :x2 y11 :y1 y21 :y2} room1
        {x12 :x1 x22 :x2 y12 :y1 y22 :y2} room2 ]
    (and (<= x11 x22)
         (>= x21 x12)
         (<= y11 y22)
         (>= y21 y12))))

;; ---------------------
(defn simple-world [map-size]
  (-> (make-map map-size)
      (carve-room (->Room 1 1 4 4))
      (carve-room (->Room 5 1 10 6))
      (carve-room (->Room 5 7 10 11))
      (carve-h-tunnel 3 5 2)
      (carve-v-tunnel 6 5 7)
      ))

(defn new-game [map-size]
  {:player (->GameObject 7 7 \@ :player)
   :objects [(->GameObject 5 5 \Z :zombie)]
   :world (simple-world map-size)})


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


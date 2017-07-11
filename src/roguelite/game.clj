(ns roguelite.game)

(defrecord GameObject [posx posy character otype])
(defrecord Tile [passable blocks-sight])
(defrecord Room [x1 y1 x2 y2])

(def torch-radius 5)


(defn lit? [[^Float cx ^Float cy] [^Float px ^Float py]]
  (< (Math/sqrt (+ (Math/pow (- cx px) 2) (Math/pow (- cy py) 2)))
     torch-radius))

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

(def room-config {:max-height 7 :max-width 7 :max-rooms 15})

(defn random-room [[max-x max-y] {:keys [max-height max-width]}]
  (let [start-x (inc (rand-int (dec max-x)))
        start-y (inc (rand-int (dec max-y)))
        end-x (min (- max-x 2) (+ start-x (rand-int max-width)))
        end-y (min (- max-y 2) (+ start-y (rand-int max-height)))]
    (->Room start-x start-y end-x end-y)))

;; ---------------------

(defn gen-rooms [map-size room-config]
  (let [max-rooms (:max-rooms room-config)]
    (loop [iters (* max-rooms 2)
           result []]
      (if (pos? iters)
        (let [new-room (random-room map-size room-config)
              intersections (filter #(intersects? % new-room) result)]
          (if (empty? intersections)
            (recur (dec iters) (conj result new-room))
            (recur (dec iters) result)))
        result))))

(defn connect-two-rooms [world room1 room2]
  (let [[cx1 cy1] (room-center room1)
        [cx2 cy2] (room-center room2)]
    (if (> (rand) 0.5)
      (-> world
          (carve-h-tunnel cx1 cx2 cy1)
          (carve-v-tunnel cx2 cy1 cy2))
      (-> world
          (carve-v-tunnel cx1 cy1 cy2)
          (carve-h-tunnel cx1 cx2 cy2))
      )))

(defn make-pairs [rooms]
  (partition 2 (interleave rooms (rest rooms))))

(defn simple-world [map-size room-config]
  (let [full-map (make-map map-size)
        rooms (gen-rooms map-size room-config)
        world-with-rooms (reduce #(carve-room %1 %2) full-map rooms)]
    (letfn [(connect [world [room1 room2]] (connect-two-rooms world room1 room2))]
      {:tiles (reduce connect world-with-rooms (make-pairs rooms))
       :rooms rooms})))

(defn new-game [map-size]
  (let [{:keys [rooms tiles]} (simple-world map-size room-config)
        [px py] (-> rooms first room-center)
        [zx zy] (-> rooms second room-center)]
    {:player (->GameObject px py \@ :player)
     :objects [(->GameObject zx zy \Z :zombie)]
     :world tiles
     :rooms rooms}))

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

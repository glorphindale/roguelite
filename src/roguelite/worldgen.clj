(ns roguelite.worldgen
  (:require [roguelite.entities :as ent]
            [roguelite.components :as comps]))

(defn make-room [x y width height]
  (ent/->Room x y (+ x width) (+ y height)))

(defn make-map [[mx my]]
  (letfn [(random-tile []
            (ent/map->Tile {:passable false :blocks-sight true :discovered false}))
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

(def room-config {:max-height 5 :max-width 5 :max-rooms 15})

(defn random-room [[map-x map-y] {:keys [max-height max-width]}]
  ;; Try to make rooms at least 2x2
  (let [start-x (inc (rand-int (- map-x 4)))
        start-y (inc (rand-int (- map-y 4)))
        width (+ 2 (rand-int (- max-width 2)))
        height (+ 2 (rand-int (- max-height 2)))
        end-x (min (+ start-x width) (dec map-x))
        end-y (min (+ start-y height) (dec map-y))]
    (ent/->Room start-x start-y end-x end-y)))

(defn random-monster [cx cy]
  (let [mtype (rand-nth [:zombie :troll :rat])
        behavior (rand-nth [:attack-nearby :roam])]  ;;; roam is kinda lame
    (ent/->GameObject cx cy mtype {:attacker {:attack (+ 1 (rand-int 2))}
                                   :defender {:defence 1 :max-hp 3 :hp 3}
                                   :sound :roar
                                   :movement behavior})))

(defn place-monster [room]
  (let [[cx cy] (room-center room)]
    (random-monster cx cy)))

(defn create-monsters [rooms]
  (vec (map place-monster rooms)))

(defn make-item [itype]
  {:itype itype})

(defn create-player [px py]
  (ent/->GameObject px py
                    :player
                    {:inventory [(make-item :health-potion) (make-item :dagger)]
                     :attacker {:attack 3}
                     :defender {:defence 1 :max-hp 10 :hp 10}}))

(defn place-potion [{:keys [x1 x2 y1 y2]}]
  (let [px (+ x1 (rand-int (- x2 x1)))
        py (+ y1 (rand-int (- y2 y1)))
        potion-type (rand-nth [:health-potion :health-potion :health-potion :attack-potion :defence-potion])]
    (ent/->GameObject px py
                      :item
                      {:passable true
                       :item-props {:itype potion-type}})))

(defn place-potions [rooms]
  (map place-potion rooms))

(defn place-player [room]
   (let [[px py] (room-center room)]
     (create-player px py)))

;; World gen

(defn make-pairs [rooms]
  (partition 2 (interleave rooms (rest rooms))))

(defn gen-rooms [map-size room-config]
  (let [max-rooms (:max-rooms room-config)]
    (loop [iters (* max-rooms 3)
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

(defn simple-world [map-size room-config]
  (let [full-map (make-map map-size)
        rooms (gen-rooms map-size room-config)
        world-with-rooms (reduce #(carve-room %1 %2) full-map rooms)]
    (letfn [(connect [world [room1 room2]] (connect-two-rooms world room1 room2))]
      {:tiles (reduce connect world-with-rooms (make-pairs rooms))
       :rooms rooms})))

(defn empty-world []
  (let [full-map (make-map [10 10])
        rooms [(make-room 2 2 3 2) (make-room 4 4 3 3)]
        with-room (reduce #(carve-room %1 %2) full-map rooms)]
    {:tiles with-room
     :rooms rooms}))

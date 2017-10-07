(ns roguelite.worldgen
  (:require [roguelite.entities :as ent]
            [roguelite.utils :as utils]
            [roguelite.components :as comps]))

;;; ===================================== Roomgen
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

;;; ===================================== Object gen
(def monsters
  {:rat {:attacker {:attack 5}
         :defender {:defence 1 :max-hp 15 :hp 15}
         :sound "squeaks"
         :movement :roam}
   :zombie {:attacker {:attack 15}
            :defender {:defence 5 :max-hp 70 :hp 70}
            :sound "growls"
            :movement :hunt}
   :troll {:attacker {:attack 25}
           :defender {:defence 5 :max-hp 40 :hp 40}
           :sound "burps"
           :movement :attack-nearby}})

(def monsters-table
  {1 {:rat 1}
   2 {:rat 3 :troll 1}
   3 {:rat 1 :troll 5 :zombie 2}
   4 {:troll 5 :zombie 5}
   5 {:troll 1 :zombie 5}
   6 {:zombie 1}})

(defn random-monster [mtype cx cy]
  (let [monster (ent/->GameObject cx cy mtype {})]
    (assoc-in monster [:components] (mtype monsters))))

(defn make-monster-tougher [monster level]
  (-> monster
      (update-in [:components :defender :hp] #(int (+ % (* 1.6 level))))
      (update-in [:components :defender :max-hp] #(int (+ % (* 1.6 level))))
      (update-in [:components :defender :defence] #(+ % (* 7 level)))
      (update-in [:components :attacker :attack] #(int (+ % (* 1.6 level))))))

(defn place-monster [available-monsters level room]
  (let [[cx cy] (room-center room)
        mtype (utils/pick-one available-monsters)
        monster (random-monster mtype cx cy)]
    (make-monster-tougher monster (dec level))))

(defn create-monsters [rooms level]
  (let [possible-mlevel (min level (->> monsters-table keys (apply max)))
        available-monsters (get monsters-table possible-mlevel)]
    (vec (map #(place-monster available-monsters level %) rooms))))

;; ============================= Items
(def items-table
  {1 {:weapon 1}
   2 {:health-potion 2 :defence-potion 1 :weapon 1}
   3 {:attack-potion 1 :health-potion 2 :defence-potion 1 :scroll 1 :armor 1}
   4 {:attack-potion 1 :health-potion 2 :scroll 1 :armor 1}
   5 {:attack-potion 1 :health-potion 2 :scroll 1 :weapon 1}
   6 {:attack-potion 1 :health-potion 2 :scroll 1 :weapon 1}})

(defn make-item [itype level]
  (let [item {:itype itype}]
    (case itype
      (:armor) (merge item {:variant :chainmail :defence (* level 5)})
      (:weapon) (merge item {:variant :dagger :attack (* level 10)})
      (:scroll) (merge item {:effect (rand-nth [:lightning :aggro :pacify])})
      item)))

(defn place-item [available-items level {:keys [x1 x2 y1 y2]}]
  (let [px (+ x1 (rand-int (- x2 x1)))
        py (+ y1 (rand-int (- y2 y1)))
        item-type (utils/pick-one available-items)
        item (ent/->GameObject px py :item {:passable true
                                            :item-props (make-item item-type level)})]
    item))

(defn place-items [rooms level]
  (let [possible-ilevel (min level (->> items-table keys (apply max)))
        available-items (get items-table possible-ilevel)]
    (vec (map #(place-item available-items level %) rooms))))

;; =============== Player
(defn create-player [px py]
  (ent/->GameObject px py
                    :player
                    {:inventory [(make-item :health-potion 1)]
                     :progression {:exp 0 :max-exp 30 :level 1}
                     :attacker {:attack 0}
                     :defender {:defence 0 :max-hp 100 :hp 100}}))

(defn place-player
  ([room]
   (let [player (create-player 0 0)]
     (place-player room player)))
  ([room player]
   (let [[px py] (room-center room)]
     (-> player
         (assoc-in [:posx] px)
         (assoc-in [:posy] py)))))

;;; ================================== World gen
(defn make-stairs []
  (ent/map->Tile {:passable true :blocks-sight false
                  :discovered false :props {:stairs true}}))

(defn place-stairs-down [tiles rooms]
  (let [last-room (last rooms)
        [cx cy] (room-center last-room)]
    (assoc-in tiles [cx cy] (make-stairs))))

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
          (carve-h-tunnel cx1 cx2 cy2)))))

(defn regular-floor [map-size room-config]
  (let [full-map (make-map map-size)
        rooms (gen-rooms map-size room-config)
        carved-map (reduce #(carve-room %1 %2) full-map rooms)]
    (letfn [(connect [world [room1 room2]] (connect-two-rooms world room1 room2))]
      {:tiles (place-stairs-down (reduce connect carved-map (make-pairs rooms)) rooms)
       :rooms rooms})))

(defn starting-floor []
  (let [full-map (make-map [10 10])
        rooms [(make-room 2 2 3 2) (make-room 4 4 3 3)]
        carved-map (reduce #(carve-room %1 %2) full-map rooms)]
    {:tiles (place-stairs-down carved-map rooms)
     :rooms rooms}))

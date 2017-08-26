(ns roguelite.fov
  (:require [roguelite.movement :as move]))

;; FOV/raycasting
(def torch-radius 5)

(defn rad-to-deg [rad]
  (-> rad (* 180) (/ Math/PI)))

(defn deg-to-rad [deg]
  (-> deg (* Math/PI) (/ 180)))

(defn dist [[^Float cx ^Float cy] [^Float px ^Float py]]
  (Math/sqrt (+ (Math/pow (- cx px) 2) (Math/pow (- cy py) 2))))

(defn lit? [[^Float cx ^Float cy] [^Float px ^Float py]]
  (< (dist [cx cy] [px py]) torch-radius))

(defn idxs-on-path [[px py] [tx ty]]
  (let [angle (-> (Math/atan2 (- ty py) (- tx px)) (* 180) (/ Math/PI))
        sx (+ px 0)
        sy (+ py 0)
        stepx (Math/cos (deg-to-rad angle))
        stepy (Math/sin (deg-to-rad angle))]
    (dedupe
      (for [step (range 1 torch-radius)
            :let [nx (Math/round (+ sx (* step stepx)))
                  ny (Math/round (+ sy (* step stepy)))]
            :while (< 0 (dist [nx ny] [tx ty]))]
        [nx ny]))))

(defn is-visible? [[px py] [tx ty] tiles]
  (let [idxs (concat (idxs-on-path [px py] [tx ty]) [[tx ty]])]
    (loop [iseq idxs
           walls-found 0
           visible true]
      (if (empty? iseq)
        visible
        (let [[x y] (first iseq)
              blocks (get-in tiles [x y :blocks-sight])
              visible (or (= walls-found 0) (not blocks))]
          (recur (rest iseq) (if blocks (+ walls-found 1) walls-found) visible))))))

(defn get-visible-tiles [[px py] tiles]
  (filter #(and (lit? [px py] %)
                (is-visible? [px py] % tiles)) (move/gen-tile-coords tiles)))

(defn update-discovered [visible-tiles world-tiles]
  (letfn [(updater-f [tiles [x y]]
            (assoc-in tiles [x y :discovered] true))]
    (reduce updater-f world-tiles visible-tiles)))

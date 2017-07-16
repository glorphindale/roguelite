(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.game :as game])
  (:import [java.awt.event KeyEvent]))

(set! *warn-on-reflection* true)

(def field-size [10 10])
(def screen-size [700 700])

;;; Input processing
(defn event->direction [event]
  (case (:key event)
    (:w :up) [0 -1]
    (:s :down) [0 1]
    (:a :left) [-1 0]
    (:d :right) [1 0]
    [0 0]))

(defn key-pressed [state event]
  (let [dir (event->direction event)
        world (:world state)]
    (case (:key event)
      (:r) (game/new-game field-size)  ;;; Restart
      (update-in state [:player] #(game/move-gobject world % dir)))))


;;;;; Drawing
(def tile-size 26)

(def tile-colors
  {:wall {true [160 160 160] false [40 40 40]}
   :floor {true [60 60 60] false [30 30 30]}
   :player [255 255 0]
   :zombie [127 255 127]})


(defn draw-gameobject [gobject]
  (q/with-fill ((:otype gobject) tile-colors)
    (let [nx (* tile-size (:posx gobject))
          ny (* tile-size (:posy gobject))]
      (q/text-char (:character gobject) nx ny))))


(defn put-tile [tile is-lit]
  (if (:passable tile)
    (q/with-fill (get-in tile-colors [:floor is-lit])
      (q/text-char \. 0 0))
    (q/with-fill (get-in tile-colors [:wall is-lit])
      (q/text-char \# 0 0))))

(defn draw-tile [tile [x y] [px py] state]
  (if (game/lit? [x y] [px py])
    (if (and (= x px) (= y py))
      (put-tile tile true)
      (let [angle (-> (Math/atan2 (- y py) (- x px))
                      (* 180)
                      (/ Math/PI))]
        (if (game/is-visible? [px py] angle (:world state))
          (do (put-tile tile true)    
              (q/with-fill [0 255 0]
                (q/rect x y 4 4)))
          (do (put-tile tile false)
              (q/with-fill [255 0 0]
                (q/rect x y 4 4))))))
    (put-tile tile false)))

(defn draw-tiles [state]
  (let [px (get-in state [:player :posx])
        py (get-in state [:player :posy])]
    (doseq [[x column] (:world state)]
      (doseq [[y tile] column]
        (q/with-translation [(* tile-size x) (* tile-size y)]
          (draw-tile tile [x y] [px py] state))))))


(defn draw-state [state]
  (q/stroke-weight 0)
  (q/background 0)

  (q/with-fill [255 255 0]
    (q/with-translation [20 20]
      (q/text (str "Player at " (get-in state [:player :posx])
                            ":" (get-in state [:player :posy])) 0 0)
      (q/text (str "Zombie at " (get-in state [:objects 0 :posx]) ":"
                                (get-in state [:objects 0 :posy])) 0 20)))

  (q/with-translation [100 100]
    (draw-tiles state)

    (let [player (:player state)
          px (:posx player)
          py (:posy player)]
      (doseq [gobject (:objects state)]
        (let [{ox :posx oy :posy} gobject
              is-lit (game/lit? [ox oy] [px py])]
          (if is-lit
            (draw-gameobject gobject))))
      (q/with-fill (:player tile-colors)
        (draw-gameobject player)))))

;;;;; Setup
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  (game/new-game field-size))

(defn update-state [state]
  state)

(q/defsketch roguelite
  :title "Roguelite"
  :size screen-size
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features []
  :key-pressed key-pressed
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn -main
  "Command-line entry point."
  [& raw-args]
  (println "Hello")
)

(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.fov :as fov] 
            [roguelite.game :as game])
  (:import [java.awt.event KeyEvent]))

(set! *warn-on-reflection* true)

(def field-size [20 20])
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
      (if (= (:raw-key event) \space)
        (assoc-in state [:state] :waiting)
        (-> state
            (assoc-in [:state] :walking)    
            (update-in [:player] #(game/move-gobject state % dir)))))))


;;;;; Drawing
(def tile-size 16)

(def tile-colors
  {:wall {true [160 160 160] false [30 30 30]}
   :floor {true [60 60 60] false [0 0 0]}
   :player [255 255 0]
   :zombie [127 0 0]
   :rat [160 0 160]
   :troll [127 255 0]})


(def otype->symb
  {:player \@
   :zombie \z
   :troll \t
   :rat \r
   :wall \#
   :floor \.})

(defn draw-gameobject [gobject]
  (q/with-fill (get-in tile-colors [(:otype gobject)] [255 255 255])
    (let [nx (* tile-size (:posx gobject))
          ny (* tile-size (:posy gobject))]
      (q/text-char (-> gobject :otype otype->symb) nx ny))))


(defn put-tile [tile is-lit]
  (if (:passable tile)
    (q/with-fill (get-in tile-colors [:floor is-lit])
      (q/text-char (otype->symb :floor) 4 -4))
    (q/with-fill (get-in tile-colors [:wall is-lit])
      (q/text-char (otype->symb :wall) 0 0))))

(defn draw-tile [tile [tx ty] [px py] state]
   (if (and (= tx px) (= ty py))
      (put-tile tile true)
      (if (some #{[tx ty]} (:visibility state))
        (put-tile tile true)   
        (if (:discovered tile) 
          (put-tile tile false)))))

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
      (case (:state state)
        (:start) (q/text (str "You see a dungeon around") 0 0)
        (:waiting) (q/text (str "You wait") 0 0)
        (:walking) (q/text (str "You take a step") 0 0)
        (q/text (str "You babble '" (:state state) "'") 0 0))
        
      #_(q/text (str "Player at " (get-in state [:player :posx])
                            ":" (get-in state [:player :posy])) 0 0)
      #_(q/text (pr-str "Monsters at " (get-in state [:objects])) 0 20)))

  (q/with-translation [100 100]
    (draw-tiles state)

    (doseq [gobject (:objects state)]
      (let [{ox :posx oy :posy} gobject]
        (if (some #{[ox oy]} (:visibility state))
          (draw-gameobject gobject))))

    (let [player (:player state)
          px (:posx player)
          py (:posy player)]
      (q/with-fill (:player tile-colors)
        (draw-gameobject player)))))

;;;;; Setup
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/text-size 16)
  ; setup function returns initial state.
  (game/new-game field-size)
  ;(game/empty-game)
  )

(defn update-state [state]
  (-> state
      (assoc-in [:visibility] (fov/get-visible-tiles 
                                [(get-in state [:player :posx]) (get-in state [:player :posy])]
                                (:world state)))
      (update-in [:world] #(fov/update-discovered (:visibility state) %))))

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

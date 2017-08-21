(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.entities :as ent]
            [roguelite.components :as comps]
            [roguelite.fov :as fov]
            [roguelite.movement :as move]
            [roguelite.game :as game])
  (:import [java.awt.event KeyEvent]))

(set! *warn-on-reflection* true)

(def field-size [35 35])
(def screen-size [1200 700])

;;; Input processing
(defn event->direction [event]
  (case (:key event)
    (:w :up) [0 -1]
    (:s :down) [0 1]
    (:a :left) [-1 0]
    (:d :right) [1 0]
    (case (long (:key-code event))
      (36) [-1 -1] (38) [0 -1] (33) [1 -1]
      (37) [-1 0]              (39) [1 0]
      (35) [-1 1]  (40) [0 1]  (34) [1 1]
      [0 0])))

(defn refresh-visibility [state]
  (if (:no-fog state)
    (-> state
        (assoc-in [:visibility] (move/gen-tile-coords (:world state)))
        (update-in [:world] #(fov/update-discovered (:visibility state) %)))
    (-> state
        (assoc-in [:visibility] (fov/get-visible-tiles
                                  [(get-in state [:player :posx]) (get-in state [:player :posy])]
                                  (:world state)))
        (update-in [:world] #(fov/update-discovered (:visibility state) %)))))

(defn process-movement [state dir]
  (-> state
      (game/one-step dir)
      (refresh-visibility)))

(defn key-pressed [state event]
  (let [dir (event->direction event)
        world (:world state)]
    (case (:state state)
      (:gameover) state
      (:use-mode) (-> state
                      (game/use-item (:key event))
                      (assoc-in [:state] :used-item)) 
      (case (:key event)
        ;;; Cheat codes for debugging
        (:O) (assoc-in state [:no-fog] true)
        (:o) (assoc-in state [:no-fog] false)
        (:u) (assoc-in state [:state] :use-mode)
        (:r) (refresh-visibility (game/new-game field-size))  ;;; Restart
        (if (= (:raw-key event) \space)
          (game/wait-step state)
          (process-movement state dir))))))


;;;;; Drawing
(def tile-size 16)

(def tile-colors
  {:wall {true [80 80 150] false [30 30 30]}
   :floor {true [60 60 60] false [0 0 0]}
   :player [255 255 0]
   :zombie [127 0 0]
   :rat [160 0 160]
   :corpse [200 200 200]
   :troll [127 255 0]})


(def otype->symb
  {:player \@
   :zombie \z
   :troll \t
   :rat \r
   :corpse \,
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

(defn- draw-healthbar [player]
  (let [hp (get-in player [:components :defender :hp])
        max-hp (get-in player [:components :defender :max-hp])
        ratio (/ hp max-hp)
        border (* 110 ratio)]
    (q/with-fill [200 0 0]
      (q/rect 50 -18 border 20 3)) 
    (q/text (str "HP: " hp "/" max-hp) 0 0)))

(def field-start [100 100])

(defn mouse-to-coords [mx my]
  (let [[startx starty] field-start
        [maxx maxy] field-size
        px (int (/ (- mx startx) tile-size))
        py (int (/ (+ tile-size (- my starty)) tile-size))]
    (if (and (>= px 0) (<= px maxx) (>= py 0) (<= py maxy))
      [px py]
      nil)))

(defn draw-state [state]
  (q/stroke-weight 0)
  (q/background 0)

  (q/with-translation field-start
    (draw-tiles state)

    (doseq [gobject (:objects state)]
      (let [{ox :posx oy :posy} gobject]
        (if (some #{[ox oy]} (:visibility state))
          (draw-gameobject gobject))))

    (let [player (:player state)
          px (:posx player)
          py (:posy player)]
      (q/with-fill (:player tile-colors)
        (draw-gameobject player))))

  ;;; Inventory
  (q/with-translation [720 400]
    (let [inventory (clojure.string/join "\n" (map-indexed vector (get-in state [:player :components :inventory])))]
      (q/text "Inventory" 0 0)
      (q/text inventory 10 20)))

  ;;; Mouse look
  (q/with-translation [(first field-start) 30]
    (when-let [coords (mouse-to-coords (q/mouse-x) (q/mouse-y))]
      (let [enumerated-tobjs (move/objects-at-pos (:objects state) coords)
            tobj (first (map second enumerated-tobjs))]
        (if tobj
          (do
            (if (get-in tobj [:components :defender])
              (q/text (str (comps/describe-defender tobj)) 30 50))
            (q/text (str "There is a " (ent/pretty-name tobj)) 30 30))))))

  (q/with-translation [720 40]
    (q/with-fill [255 255 255]
      (draw-healthbar (:player state))
      (q/text (str "Attack: " (get-in state [:player :components :attacker :attack])) 0 20)
      (q/text (str "Defence: " (get-in state [:player :components :defender :defence])) 0 40))

    (q/with-fill [255 255 0]
      (q/with-translation [0 80]
        (case (:state state)
          (:start) (q/text (str "You see a dungeon around") 0 0)
          (:waiting) (q/text (str "You wait") 0 0)
          (:use-mode) (q/text (str "Select an item to use 1-9") 0 0)
          (:walking) (q/text (str "You take a step") 0 0)
          (:attacking) (q/text (str "You attack!") 0 0)
          (:gameover) (q/text (str "You are slain! Press R to restart.") 0 0)
          (q/text (str "Current mode " (:state state)) 0 0))
        (let [messages (filter (complement nil?) (flatten (:messages state)))]
          (q/text (str (clojure.string/join "\n" messages)) 0 25)))))
  )

;;;;; Setup
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (let [font (q/load-font "DFBisasam16x16-16.vlw")]
    (q/text-font font 16))
  (refresh-visibility (game/simple-game)))

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

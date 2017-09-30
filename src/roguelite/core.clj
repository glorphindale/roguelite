(ns roguelite.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [roguelite.entities :as ent]
            [roguelite.components :as comps]
            [roguelite.movement :as move]
            [roguelite.game :as game]
            [roguelite.saving :as saving])
  (:import [java.awt.event KeyEvent]))

(set! *warn-on-reflection* true)

(def screen-size [1500 700])

(defn init-game []
  {:state :menu :selected 0})

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

(defn process-movement [state dir]
  (-> state
      (game/one-step dir)
      (game/refresh-visibility)))

(defn key-pressed-int [state event]
  (let [dir (event->direction event)
        world (:world state)]
    (case (:state state)
      (:gameover) (do
                    (saving/delete-save init-game)
                    (if (= (:key event) :r)
                      (game/inject-player (game/simple-game))
                      state))
      (:use-mode) (-> state
                      (game/use-item (:key event))
                      (assoc-in [:state] :used-item))
      (:drop-mode) (-> state
                       (game/drop-item (:key event))
                       (assoc-in [:state] :dropped-item))
      (case (:key event)
        ;;; Cheat codes for debugging
        (:O) (assoc-in state [:no-fog] true)
        (:o) (assoc-in state [:no-fog] false)
        (:u) (assoc-in state [:state] :use-mode)
        (:d) (assoc-in state [:state] :drop-mode)
        (:>) (game/try-descend state)
        (:S) (do
               (saving/save-game state)
               (ent/+msg state "Game saved"))
        (:p) (game/pickup state)
        (:r) (game/inject-player (game/simple-game))  ;;; Restart
        (case (long (:key-code event))
          (32 10) (game/wait-step state)
          (33 34 35 36 37 38 39 40) (process-movement state dir)
          state)))))

(defn key-pressed-menu [state event]
  (if (= (:raw-key event) \newline)
    (case (long (:selected state))
      0 (game/inject-player (game/simple-game))
      1 (if (saving/has-save)
           (saving/load-game)
          state))
    (case (:key event)
      (:down) (update-in state [:selected] #(min 1 (inc %))) 
      (:up)   (update-in state [:selected] #(max 0 (dec %)))
      state)))

(defn key-pressed [state event]
  (try
    (case (:state state)
      (:menu) (key-pressed-menu state event)
      (key-pressed-int state event))
    (catch Exception e (q/text (str "Exception: " e) 20 20))))

;;;;; Drawing
(def tile-size 16)

(def tile-colors
  {:wall {true [80 80 150] false [30 30 30]}
   :floor {true [60 60 60] false [0 0 0]}
   :stairs {true [60 60 60] false [60 60 60]}
   :player [255 255 0]
   :zombie [127 0 0]
   :rat [160 0 160]
   :item [255 127 127]
   :corpse [200 200 200]
   :troll [127 255 0]})


(def otype->symb
  {:player \@
   :zombie \z
   :troll \t
   :rat \r
   :corpse \,
   :item \*
   :wall \#
   :stairs \>
   :floor \.})

(defn draw-gameobject [gobject]
  (q/with-fill (get-in tile-colors [(:otype gobject)] [255 255 255])
    (let [nx (* tile-size (:posx gobject))
          ny (* tile-size (:posy gobject))]
      (q/text-char (-> gobject :otype otype->symb) nx ny))))


(defn put-tile [tile is-lit]
  (if (get-in tile [:props :stairs] false)
    (q/with-fill (get-in tile-colors [:stairs is-lit])
      (q/text-char (otype->symb :stairs) 4 -4))
    (if (:passable tile)
      (q/with-fill (get-in tile-colors [:floor is-lit])
        (q/text-char (otype->symb :floor) 4 -4))
      (q/with-fill (get-in tile-colors [:wall is-lit])
        (q/text-char (otype->symb :wall) 0 0)))))

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

(defn- draw-xpbar [player]
  (let [{:keys [level exp max-exp]} (get-in player [:components :progression])
        ratio (/ exp max-exp)
        border (* 120 ratio)]
    (q/with-fill [40 40 40]
      (q/rect 58 -18 124 22 3))
    (q/with-fill [0 200 0]
      (q/rect 60 -16 border 18 3))
    (q/text (str "XP: " exp "/" max-exp) 0 0)))

(defn- draw-healthbar [player]
  (let [hp (get-in player [:components :defender :hp])
        max-hp (get-in player [:components :defender :max-hp])
        ratio (/ hp max-hp)
        border (* 120 ratio)]
    (q/with-fill [40 40 40]
      (q/rect 58 -18 124 22 3))
    (q/with-fill [200 0 0]
      (q/rect 60 -16 border 18 3)) 
    (q/text (str "HP: " hp "/" max-hp) 0 0)))

(def field-start [100 80])

(defn mouse-to-coords [mx my]
  (let [[startx starty] field-start
        [maxx maxy] game/field-size
        px (int (/ (- mx startx) tile-size))
        py (int (/ (+ tile-size (- my starty)) tile-size))]
    (if (and (>= px 0) (<= px maxx) (>= py 0) (<= py maxy))
      [px py]
      nil)))

(defn draw-gameplay [state]
  (q/background 0)
  (q/stroke-weight 2)
  ;;; Mouse look
  (when-let [[x y] (mouse-to-coords (q/mouse-x) (q/mouse-y))]
    (when (some #{[x y]} (:visibility state))
      (do
        (q/with-fill [100 100 100]
          (q/with-translation field-start
            (q/rect (* x tile-size) (+ (- tile-size) (* y tile-size)) tile-size tile-size)))
        (let [enumerated-tobjs (move/objects-at-pos (:objects state) [x y])
              tobj (first (map second enumerated-tobjs))]
          (when tobj
            (q/with-translation [x 10]
              (q/with-fill [255 255 255]
                (q/text (comps/describe-obj tobj) 30 30))))))))

  (q/stroke-weight 0)
  ;;; Draw field
  (q/with-translation field-start
    (draw-tiles state)

    (doseq [gobject (:objects state)]
      (let [{ox :posx oy :posy} gobject]
        (if (some #{[ox oy]} (:visibility state))
          (draw-gameobject gobject))))

    (let [player (:player state)]
      (q/with-fill (:player tile-colors)
        (draw-gameobject player))))

  (q/with-translation [100 650]
    (q/text "a/w/s/d/arrows to move and attack, spacebar to wait" 0 0)
    (q/text "'u' to use an item, 'p' to pickup an item " 0 20)
    (q/text "'S' to save, '>' to go downstairs" 0 40))

  ;;; Inventory
  (q/with-translation [720 460]
    (let [inventory (clojure.string/join "\n"
                                         (map #(str (inc (first %)) " " (-> % second comps/describe-item))
                                              (map-indexed vector (get-in state [:player :components :inventory]))))]
      (q/text "Inventory" 0 0)
      (q/text inventory 10 20)))

  (q/with-translation [720 40]
    (q/with-fill [255 255 255]
      (q/with-translation [0 0]
        (draw-healthbar (:player state)))
      (q/with-translation [0 20]
       (draw-xpbar (:player state)))
      (q/text (str "Attack: " (get-in state [:player :components :attacker :attack])) 0 40)
      (q/text (str "Defence: " (get-in state [:player :components :defender :defence])) 0 60)
      (q/text (str "Level " (get-in state [:level])) 0 80))

    (q/with-translation [0 100]
      (q/with-fill [255 200 100]
        (case (:state state)
          (:start) (q/text (str "You see a dungeon around") 0 0)
          (:waiting) (q/text (str "You wait") 0 0)
          (:use-mode) (q/text (str "Select an item to use 0-9") 0 0)
          (:drop-mode) (q/text (str "Select an item to drop 0-9") 0 0)
          (:walking) (q/text (str "You take a step") 0 0)
          (:attacking) (q/text (str "You attack!") 0 0)
          (:gameover) (q/with-fill [255 0 0] (q/text (str "You are slain!\nPress R to restart.") 0 0))
          (q/text (str "") 0 0)))
      (q/with-fill [255 255 0]
        (let [messages (filter (complement nil?) (flatten (:messages state)))]
          (q/text (str (clojure.string/join "\n" (take-last 18 messages))) 0 32))))))

(defn draw-menu [state]
  (q/background 0)
  (let [[w h] screen-size
        cx (- (/ w 2) 100)
        cy (/ h 2)
        marker-y (* 20 (:selected state))]
    (q/with-translation [cx cy]
      (q/with-fill [200 100 100]
        (q/text "Dungeons of Clojurelike" -40 -20))
      (q/with-fill [200 0 0]
        (q/text "New game" 0 0)
        (if (saving/has-save)
          (q/text "Load previous game" 0 20) 
          (q/with-fill [100 100 100]
            (q/text "Load previous game" 0 20)))
        (q/text "->" -40 marker-y)))))

(defn draw-state [state]
  (case (:state state)
    (:menu) (draw-menu state)
    (draw-gameplay state)))

;;;;; Setup
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (let [font (q/load-font "DFBisasam16x16-16.vlw")]
    (q/text-font font 16))
  (init-game))

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

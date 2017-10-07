(ns roguelite.entities)

(defrecord Room [x1 y1 x2 y2])
(defrecord Tile [passable blocks-sight discovered props])
(defrecord GameObject [posx posy otype components])

(defn pretty-value [value]
  (-> value name clojure.string/capitalize))

(defn pretty-name [gobject]
  (-> gobject :otype name clojure.string/capitalize))

(defn +msg [state message]
  (update-in state [:messages] conj message))

(defn get-pos [gobject]
  [(:posx gobject) (:posy gobject)])

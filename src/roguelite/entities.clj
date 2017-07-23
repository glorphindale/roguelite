(ns roguelite.entities)

(defrecord Room [x1 y1 x2 y2])
(defrecord Tile [passable blocks-sight discovered])
(defrecord GameObject [posx posy otype components])

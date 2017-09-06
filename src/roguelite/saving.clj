(ns roguelite.saving
  (require [clojure.edn :as edn]
           [roguelite.entities :as ent]))

(def fname "savefile.clj")

(defn save-game [state]
  (spit fname (pr-str state)))

(def edn-readers {'roguelite.entities.GameObject ent/map->GameObject
                  'roguelite.entities.Tile ent/map->Tile  
                  'roguelite.entities.Room ent/map->Room})

(defn load-game []
  (edn/read-string {:readers edn-readers} (slurp fname)))

(defn delete-save [func]
  (save-game (func)))

(defn has-save []
  (.exists (clojure.java.io/as-file fname)))

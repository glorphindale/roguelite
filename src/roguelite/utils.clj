(ns roguelite.utils)

(defn dist [[^Float cx ^Float cy] [^Float px ^Float py]]
  (Math/sqrt (+ (Math/pow (- cx px) 2) (Math/pow (- cy py) 2))))


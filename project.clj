(defproject roguelite "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main roguelite.core
  :profiles {:dev {:dependencies [[jonase/eastwood "0.2.4" :exclusions [org.clojure/clojure]]]}}
  :dependencies [[org.clojure/clojure "1.9.0-beta1"]
                 [quil "2.6.0"]])

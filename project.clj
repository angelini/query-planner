(defproject query-planner "0.1.0-SNAPSHOT"
  :url "http://github.com/angelini/query-planner"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/schema "0.4.4"]]
  :plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]]
  :main ^:skip-aot query-planner.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

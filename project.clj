(defproject ysmathemagick "0.1.0-SNAPSHOT"
  :description "my math exercises in clojure."
  :url "https://github.com/ysmiraak/ysmathemagick"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha13"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [net.mikera/core.matrix "0.55.0"]
                 [anglican "1.0.0"]
                 [incanter "1.9.1"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})

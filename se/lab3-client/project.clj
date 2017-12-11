(defproject lab3-client "1.0.0"
  :description "SE Lab 3 client-side script"
  :url "http://github.com/thymelous/ifmo"
  :license {:name "CC0 1.0"
            :url "https://creativecommons.org/publicdomain/zero/1.0/legalcode"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [clj-http "3.7.0"]
                 [cheshire "5.8.0"]
                 [clj-jwt "0.1.1"]]
  :main ^:skip-aot lab3-client.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

(defproject ping-pong "0.1.0"
  :description "Clojure ping-pong server for Clojang/LFE example."
  :url "http://github.com/oubiwann/clojang"
  :license {:name "Apache License Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.typed "0.3.22"]
                 [twig "0.1.2"]
                 [clojang "0.1.0-dev"]]
  :main ping-pong.core)

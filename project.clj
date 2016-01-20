(defproject clojang "0.1.0"
  :description " Erlang's JInterface in Idiomatic Clojure"
  :url "https://github.com/oubiwann/clojang"
  :scm {:name "git"
        :url "https://github.com/oubiwann/clojang"}
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.erlang.otp/jinterface "1.6.1"]]
  :plugins [[lein-codox "0.9.1"]
            [lein-simpleton "1.3.0"]]
  :test-selectors {:default :unit
                   :unit :unit
                   :system :system
                   :integration :integration}
  :codox {:output-path "docs/master/current"
          :doc-paths ["docs/source"]})


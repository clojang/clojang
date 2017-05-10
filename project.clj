(defproject clojang "0.4.0-SNAPSHOT"
  :description "Erlang/OTP Communications in Clojure (wraps jiface + JInterface)"
  :url "https://github.com/clojang/clojang"
  :scm {
    :name "git"
    :url  "https://github.com/clojang/clojang"}
  :license {
    :name "Apache License, Version 2.0"
    :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [
    [org.clojure/clojure "1.8.0"]
    [org.clojure/core.memoize "0.5.9"]
    [clojang/agent "0.4.0-SNAPSHOT"]
    [clojang/jiface "0.4.0-SNAPSHOT"]
    [clojusc/trifl "0.1.0-SNAPSHOT"]
    [clojusc/twig "0.3.1"]]
  :source-paths ["src/clojure"]
  :codox {
    :project {:name "clojang"}
    :themes [:clojang]
    :output-path "docs/current"
    :doc-paths ["resources/docs"]
    :namespaces [#"^clojang\.(?!test)"
                 #"^clojang\.(?!agent)"]
    :metadata {:doc/format :markdown}}
  :profiles {
    :uberjar {:aot :all}
    :testing {
       :plugins
         [[lein-ancient "0.6.10"]
          [jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]
          [lein-bikeshed "0.4.1"]
          [lein-kibit "0.1.4" :exclusions [org.clojure/clojure]]
          [venantius/yagni "0.1.4"]]
      :aot :all
      :dependencies [
        [org.clojure/math.numeric-tower "0.0.4"]]
      :jvm-opts ["-Dheadless"]
      :test-selectors {
        :default :unit
        :unit :unit
        :system :system
        :integration :integration}}
    :docs {
      :aot :all
      :dependencies [[clojang/codox-theme "0.2.0-SNAPSHOT"]]
      :plugins [
        [lein-codox "0.10.3"]
        [lein-simpleton "1.3.0"]]}
    :dev {
      :dependencies [
        [org.clojure/tools.namespace "0.2.11"]]
      :source-paths ["dev-resources/src"]
      :jvm-opts [
        ;"-verbose:class"
        ;"-Dheadless"
        "-splash:resources/images/logo-5-250x.png"
        "-Dnode.sname=clojang"]
      :java-agents [[clojang/agent "0.4.0-SNAPSHOT"]]
      :repl-options {:init-ns clojang.dev}}})

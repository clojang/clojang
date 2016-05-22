(defproject clojang "0.2.0-SNAPSHOT"
  :description "Erlang's JInterface in Idiomatic Clojure"
  :url "https://github.com/clojang/clojang"
  :scm {
    :name "git"
    :url  "https://github.com/clojang/clojang"}
  :license {
    :name "Apache License, Version 2.0"
    :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [
    [org.clojure/core.memoize "0.5.8"]
    [clojang/jiface "0.1.2-SNAPSHOT"]]
  :source-paths ["src/clojure"]
  :profiles {
    ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    :testing {
      :aot :all
      :dependencies [
        [org.clojure/math.numeric-tower "0.0.4"]
        [twig "0.1.6"]]
      :source-paths ["test"]
      :test-selectors {
      :default :unit
      :unit :unit
      :system :system
      :integration :integration}}
    ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    :docs {
      :plugins [
        [lein-codox "0.9.5"]
        [lein-simpleton "1.3.0"]]
      :dependencies [[twig "0.1.6"]]
      :codox {
        :output-path "docs/master/current"
        :doc-paths ["docs/source"]
        :namespaces [#"^clojang\.(?!test)"
                     #"^clojang\.(?!agent)"]
        :metadata {:doc/format :markdown}}}
    ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    :dev {
      :dependencies [
        [clojang/agent "0.1.4-SNAPSHOT"]
        [org.clojure/tools.namespace "0.2.11"]
        [twig "0.1.6"]]
      :source-paths ["dev-resources/src"]
      :jvm-opts [
        "-splash:resources/images/clojang-logo-250x.png"
        "-Dnode.sname=clojang"
        "-Dheadless"]
      :java-agents [[clojang/agent "0.1.4-SNAPSHOT"]]
      :repl-options {:init-ns clojang.dev}}})

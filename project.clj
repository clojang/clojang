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
    [org.clojure/clojure "1.8.0"]
    [org.clojure/core.memoize "0.5.9"]
    [clojang/agent "0.2.0-SNAPSHOT"]
    [clojang/jiface "0.2.0-SNAPSHOT"]
    [clojusc/twig "0.3.0"]]
  :source-paths ["src/clojure"]
  :profiles {
    ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    :test {
      :aot :all
      :dependencies [
        [org.clojure/math.numeric-tower "0.0.4"]]
      :jvm-opts ["-Dheadless"]
      :source-paths ["test"]
      :test-selectors {
      :default :unit
      :unit :unit
      :system :system
      :integration :integration}}
    ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    :docs {
      :plugins [
        [lein-codox "0.10.2"]
        [lein-simpleton "1.3.0"]]
      :codox {
        :output-path "docs/master/current"
        :doc-paths ["docs/source"]
        :namespaces [#"^clojang\.(?!test)"
                     #"^clojang\.(?!agent)"]
        :metadata {:doc/format :markdown}}}
    ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    :dev {
      :dependencies [
        [org.clojure/tools.namespace "0.2.11"]]
      :source-paths ["dev-resources/src"]
      :jvm-opts [
        ;"-verbose:class"
        ;"-Dheadless"
        "-splash:resources/images/logo-5-250x.png"
        "-Dnode.sname=clojang"]
      :java-agents [[clojang/agent "0.2.0-SNAPSHOT"]]
      :repl-options {:init-ns clojang.dev}}})

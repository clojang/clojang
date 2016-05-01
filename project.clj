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
    [clojang/jiface "0.1.2-SNAPSHOT"]
    [clojang/agent "0.1.3-SNAPSHOT"]]
  :plugins [
    [lein-codox "0.9.5"]
    [lein-simpleton "1.3.0"]]
  :source-paths ["src/clojure"]
  :jvm-opts ["-Dnode.sname=clojang"]
  :java-agents [[clojang/agent "0.1.3-SNAPSHOT"]]
  :test-selectors {
    :default :unit
    :unit :unit
    :system :system
    :integration :integration}
  :codox {
    :output-path "docs/master/current"
    :doc-paths ["docs/source"]
    :namespaces [#"^clojang\.(?!test)"]
    :metadata {:doc/format :markdown}}
  :profiles {
    :testing {
      :aot :all
      :dependencies [
        [org.clojure/math.numeric-tower "0.0.4"]
        [twig "0.1.6"]]
      :source-paths ["test", "dev-resources/src"]}
    :dev {
      :dependencies [
        [org.clojure/tools.namespace "0.2.11"]
        [twig "0.1.6"]]
      :source-paths ["dev-resources/src"]
      :jvm-opts ["-splash:resources/images/clojang-logo-250x.png"]
      :aot [clojure.tools.logging.impl]
      :repl-options {:init-ns clojang.dev}}})

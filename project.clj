(defproject clojang "0.5.0-SNAPSHOT"
  :description "Erlang/OTP Communications in Clojure (wraps jiface + JInterface)"
  :url "https://github.com/clojang/clojang"
  :scm {
    :name "git"
    :url  "https://github.com/clojang/clojang"}
  :license {
    :name "Apache License, Version 2.0"
    :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :exclusions [
    [org.clojure/clojure]
    [org.clojure/tools.namespace]
    [org.clojure/tools.reader]]
  :dependencies [
    [clojang/agent "0.4.0"]
    [clojang/jiface "0.4.0"]
    [clojusc/trifl "0.3.0"]
    [org.clojure/clojure "1.9.0"]
    [org.clojure/core.memoize "0.7.1"]
    [org.clojure/tools.namespace "0.2.11"]
    [org.clojure/tools.reader "1.3.0"]]
  :source-paths ["src/clojure"]
  :aot :all
  :codox {
    :project {:name "clojang"}
    :themes [:clojang]
    :output-path "docs/current"
    :doc-paths ["resources/docs"]
    :namespaces [#"^clojang\.(?!test)"]
    :metadata {:doc/format :markdown}}
  :profiles {
    :ubercompile {
      :aot :all}
    :lint {
      :source-paths ^:replace ["src"]
      :test-paths ^:replace []
      :plugins [
        [jonase/eastwood "0.2.8"]
        [lein-ancient "0.6.15"]
        [lein-kibit "0.1.6"]
        [venantius/yagni "0.1.4"]]}
    :test {
      :aot :all
      :dependencies [
        [org.clojure/math.numeric-tower "0.0.4"]]
      :plugins [
        [lein-ltest "0.3.0"]]
      :source-paths ["test"]
      :jvm-opts ["-Dheadless"]
      :test-selectors {
        :default :unit
        :unit :unit
        :system :system
        :integration :integration}}
    :docs {
      :aot :all
      :dependencies [
        [clojang/codox-theme "0.2.0-SNAPSHOT"]]
      :plugins [
        [lein-codox "0.10.4"]
        [lein-simpleton "1.3.0"]]}
    :dev {
      :aot :all
      :source-paths ["dev-resources/src"]
      :repl-options {:init-ns clojang.dev}
      :bootclasspath true
      :java-agents [
        [clojang/agent "0.4.0"]]
      :jvm-opts [
        ; "-verbose:class"
        ;"-Dheadless"
        "-splash:resources/images/logo-5-250x.png"
        "-Dnode.sname=clojang"
        ;"-javaagent:resources/jars/clojang/agent.jar"
        ]}}
  :aliases {
    ;; Dev Aliases
    "repl-clean" ["do"
      ["clean"]
      ["repl"]]
    "ubercompile" ["do"
      ["clean"]
      ["with-profile" "+ubercompile" "compile"]]
    "check-vers" ["with-profile" "+lint" "ancient" "check" ":all"]
    "check-jars" ["with-profile" "+lint" "do"
      ["deps" ":tree"]
      ["deps" ":plugin-tree"]]
    "check-deps" ["do"
      ["check-jars"]
      ["check-vers"]]
    "kibit" ["with-profile" "+lint" "kibit"]
    "eastwood" ["with-profile" "+lint" "eastwood" "{:namespaces [:source-paths]}"]
    "lint" ["do"
      ["kibit"]
      ;["eastwood"]
      ]
    "ltest" ["with-profile" "+test" "ltest"]
    "ltest-clean" ["do"
      ["clean"]
      ["ltest"]]})

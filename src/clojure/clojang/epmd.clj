(ns clojang.epmd
  (:require [clojang.caller :refer [call call!]]
            [clojure.java.shell :as shell]
            [jiface.epmd :as ji-epmd]
            [dire.core :refer [with-handler!]]
            [potemkin :refer [import-vars]]))

(defn -epmd-parse-args
  [[k v]]
  (let [str-val (str v)]
    (case k
      :debug ["-debug"]
      :package-timeout ["-packet_timeout" str-val]
      :delay-accept ["-delay_accept" str-val]
      :delay-write ["-delay_write" str-val]
      :address ["-address" (clojure.string/join "," v)]
      :port ["-port" str-val]
      :daemon ["-daemon"]
      :relaxed-command-check ["-relaxed_command_check"]
      :names ["-names"]
      :kill ["-kill"]
      :stop ["-stop" str-val])))

(defn epmd
  "Either start or interact with the Erlang Port Mapper Daemon external (OS)
  process needed by JInterface for creating nodes and communicating with other
  nodes.

  Usage based on documentation here:
   * http://erlang.org/doc/man/epmd.html#debug_flags

  Notes:
   * Single-valued command line arguments expect a `true` value in this
     function
   * Command line arguments with underscores are keywords with dashes in this
     function

  Example usage:

  ```
  clojang.dev=> (epmd :names true)
  epmd: up and running on port 4369 with data:
  name clojang at port 33681
  name clojang-lfe at port 40968
  name rabbit at port 25672
  :ok
  ```"
  [& {:as args}]
  (->> args
       (map -epmd-parse-args)
       (flatten)
       (cons "epmd")
       (apply shell/sh)
       :out
       (print))
  :ok)

(defn- -parse-name
  [result]
  (->> result
       (re-matches #"name (.*) at port (.*)")
       (#(vector (get % 1) (Integer/parseInt (get % 2))))))

(defn- -parse-names [results]
  (->> results
       (map -parse-name)
       vec))

(defn lookup-names
  ([]
    (->> (call ji-epmd/lookup-names)
         (-parse-names)))
  ([inet-addr-str]
    (->> (java.net.InetAddress/getByName inet-addr-str)
         (call ji-epmd/lookup-names)
         (-parse-names)))
  ([inet-addr-str transport]
    (->> (java.net.InetAddress/getByName inet-addr-str)
         (into [transport])
         (call ji-epmd/lookup-names)
         (-parse-names))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Aliases   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [ji-epmd
   ;; epmd
   ;; lookup-names -- see above
   lookup-port
   publish-port
   unpublish-port
   use-port])

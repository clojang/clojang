(ns clojang.util
  (:require [clojure.string :as string]
            [dire.core :refer [with-handler!]])
  (:import [clojure.lang Reflector]
           [com.ericsson.otp.erlang]))

;; XXX support the following keys:
;; [-d|-debug] [DbgExtra...] [-port No] [-daemon] [-relaxed_command_check]
(defn start-epmd
  "Start the Erlang Port Mapper Daemon external (OS) process needed by
  JInterface for creating nodes and communicating with other nodes."
  []
  'TBD)

(defn convert-class-name
  "A helper function for use when creating Erlang class wrappers."
  [name-symbol]
  (case (str name-symbol)
    ;; Types
    "external-fun" "ExternalFun"
    "list-sublist" "List$SubList"
    "object-hash" "Object$Hash"
    "uint" "UInt"
    "ushort" "UShort"
    ;; OTP
    "local-node" "LocalNode"
    ;; Everything else
    (string/capitalize name-symbol)))

(defn make-jinterface-name
  "A helper function for use when defining constructor macros."
  [prefix name-symbol]
  (->> name-symbol
       (str)
       (convert-class-name)
       (str "com.ericsson.otp.erlang." prefix)
       (symbol)))

(defn dynamic-init
  "Dynamically instantiates classes based upon a transformation function and
  a symbol used by the transformation function to create a class name that is
  ultimately resolvable."
  [name-gen-fn name-part & args]
    (Reflector/invokeConstructor
      (resolve (name-gen-fn name-part))
      (into-array Object args)))

(defn get-hostname
  "Get the hostname for the machine that this JVM is running on.

  Uses the ``java.net.InetAddress`` methods ``getLocalHost`` and
  ``getHostName``."
  []
  (-> (java.net.InetAddress/getLocalHost)
      (.getHostName)))

(defn add-err-handler
  "A wrapper for generating a specific dire error handler."
  ([handled-fn excep]
    (add-err-handler
      handled-fn
      excep
      "[ERROR] There was a problem!"))
  ([handled-fn excep msg]
    (with-handler! handled-fn
      excep
      (fn [e & args]
        (println msg)
        (println (str {:args args :errors e}))))))

(defn ->str-arg [arg]
  (condp #(%1 %2) arg
    keyword? (name arg)
    symbol? (str arg)
    arg))

(defn ->str-args [args]
  (reduce
    (fn [acc x]
      (into acc [x]))
    []
    (map ->str-arg args)))

(defn defined?
  [item]
  (cond
    empty? false
    nil? false))

(defn close-splash-screen
  ""
  []
  (-> (java.awt.SplashScreen/getSplashScreen)
      (.close)))

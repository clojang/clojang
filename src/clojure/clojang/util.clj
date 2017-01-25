(ns clojang.util
  (:require [clojure.string :as string]
            [dire.core :refer [with-handler!]])
  (:import [clojure.lang Reflector]
           [com.ericsson.otp.erlang]))

(defn ->str-arg
  [arg]
  (condp #(%1 %2) arg
    keyword? (name arg)
    symbol? (str arg)
    arg))

(defn ->str-args
  [args]
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

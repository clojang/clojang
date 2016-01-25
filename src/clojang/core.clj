(ns clojang.core
  (:require [clojure.string :as clj-string]
            [dire.core :refer [with-handler!]]
            [potemkin :refer [import-vars]]
            [clojang.jinterface.erlang.atom :as atom]
            [clojang.jinterface.erlang.boolean :as boolean]
            [clojang.jinterface.erlang.string :as string]
            [clojang.jinterface.erlang.types :as types]
            [clojang.jinterface.otp.nodes]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean]))

(import-vars
  [clojang.jinterface.otp.nodes

   node
   peer
   ; get-*
   ; create-*
   ; register-*
   ; set-*
   ping
   whereis]

  ;;[clojang.jinterface.otp.messaging
  ;;
  ;; register-name]

   )

(defprotocol EDNConverter
  "Convert EDN."
  (edn->term [this] "Convert Clojure EDN to JInterface Erlang types."))

(extend-protocol EDNConverter
  ;; nil
  nil
  (edn->term [edn]
    (types/atom "undefined"))
  ;; atom / keyword
  clojure.lang.Keyword
  (edn->term [edn]
    (types/atom (name edn)))
  ;; boolean
  java.lang.Boolean
  (edn->term [edn]
    (types/boolean edn))
  ;; tuple /vector
  ;; list
  ;; string
  java.lang.String
  (edn->term [edn]
    (types/string edn)))

(defprotocol TermConverter
  "Convert JInterface Erlang terms."
  (term->edn [this] "Convert JInterface Erlang types to EDN."))

(extend-protocol TermConverter
  ;; atom / keyword & undefined
  com.ericsson.otp.erlang.OtpErlangAtom
  (term->edn [erl-obj]
    (let [trans (atom/->str erl-obj)]
      (if (= trans "undefined")
        nil
        (keyword trans))))
  ;; boolean
  com.ericsson.otp.erlang.OtpErlangBoolean
  (term->edn [erl-obj]
    (boolean/get-value erl-obj))
  ;; tuple /vector
  ;; list
  ;; string
  com.ericsson.otp.erlang.OtpErlangString
  (term->edn [erl-obj]
    (clj-string/replace
      (string/->str erl-obj)
      #"\""
      "")))

;;; Aliases

(def ->erlang #'edn->term)
(def ->clojure #'term->edn)

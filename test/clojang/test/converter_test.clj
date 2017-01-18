(ns clojang.test.converter-test
  (:require [clojure.test :refer :all]
            [dire.core :refer [with-handler!]]
            [clojang.types.converter :as converter]
            [jiface.erlang.atom :as atom]
            [jiface.erlang.atom :as boolean]
            [jiface.erlang.string :as string]
            [jiface.erlang.types :as types])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean]))

(deftest ^:unit clj->erl-test
  ;; nil
  (let [term (converter/clj->erl nil)]
    (is (= com.ericsson.otp.erlang.OtpErlangAtom (type term)))
    (is (= "undefined" (atom/->str term))))
  ;; atom / keyword
  (let [term (converter/clj->erl :a)]
    (is (= com.ericsson.otp.erlang.OtpErlangAtom (type term)))
    (is (= "a" (atom/->str term))))
  ;; boolean
  (let [term (converter/clj->erl true)]
    (is (= com.ericsson.otp.erlang.OtpErlangBoolean (type term)))
    (is (= "true" (boolean/->str term))))
  ;; tuple /vector
  ;; list
  ;; string
  (let [term (converter/clj->erl "a")]
    (is (= com.ericsson.otp.erlang.OtpErlangString (type term)))
    (is (= "\"a\"" (string/->str term)))))

(deftest ^:unit erl->clj-test
  ;; nil
  (let [edn (converter/erl->clj (types/atom "undefined"))]
    (is (= nil (type edn)))
    (is (= nil edn)))
  ;; atom / keyword
  (let [edn (converter/erl->clj (types/atom "a"))]
    (is (= clojure.lang.Keyword (type edn)))
    (is (= :a edn)))
  ;; boolean
  (let [edn (converter/erl->clj (types/boolean true))]
    (is (= java.lang.Boolean (type edn)))
    (is (= true edn)))
  ;; tuple /vector
  ;; list
  ;; string
  (let [edn (converter/erl->clj (types/string "a"))]
    (is (= java.lang.String (type edn)))
    (is (= "a" edn))))




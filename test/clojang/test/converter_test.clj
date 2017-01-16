(ns clojang.test.converter-test
  (:require [clojure.test :refer :all]
            [dire.core :refer [with-handler!]]
            [clojang.converter :as converter]
            [jiface.erlang.atom :as atom]
            [jiface.erlang.atom :as boolean]
            [jiface.erlang.string :as string]
            [jiface.erlang.types :as types])
  (:import [com.ericsson.otp.erlang])
  (:refer-clojure :exclude [atom boolean]))

(deftest ^:unit clj->term-test
  ;; nil
  (let [term (converter/clj->term nil)]
    (is (= com.ericsson.otp.erlang.OtpErlangAtom (type term)))
    (is (= "undefined" (atom/->str term))))
  ;; atom / keyword
  (let [term (converter/clj->term :a)]
    (is (= com.ericsson.otp.erlang.OtpErlangAtom (type term)))
    (is (= "a" (atom/->str term))))
  ;; boolean
  (let [term (converter/clj->term true)]
    (is (= com.ericsson.otp.erlang.OtpErlangBoolean (type term)))
    (is (= "true" (boolean/->str term))))
  ;; tuple /vector
  ;; list
  ;; string
  (let [term (converter/clj->term "a")]
    (is (= com.ericsson.otp.erlang.OtpErlangString (type term)))
    (is (= "\"a\"" (string/->str term)))))

(deftest ^:unit term->clj-test
  ;; nil
  (let [edn (converter/term->clj (types/atom "undefined"))]
    (is (= nil (type edn)))
    (is (= nil edn)))
  ;; atom / keyword
  (let [edn (converter/term->clj (types/atom "a"))]
    (is (= clojure.lang.Keyword (type edn)))
    (is (= :a edn)))
  ;; boolean
  (let [edn (converter/term->clj (types/boolean true))]
    (is (= java.lang.Boolean (type edn)))
    (is (= true edn)))
  ;; tuple /vector
  ;; list
  ;; string
  (let [edn (converter/term->clj (types/string "a"))]
    (is (= java.lang.String (type edn)))
    (is (= "a" edn))))




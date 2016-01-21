(ns clojang.jinterface.erlang.types-test
  (:require [clojure.test :refer :all]
            [clojang.jinterface.erlang.types :as types])
  (:import [com.ericsson.otp.erlang
            OtpErlangAtom
            OtpErlangBinary
            OtpErlangBitstr
            OtpErlangBoolean
            OtpErlangByte
            OtpErlangChar
            OtpErlangDouble
            OtpErlangExternalFun
            OtpErlangFloat
            OtpErlangFun
            OtpErlangInt
            OtpErlangList
            OtpErlangList$SubList
            OtpErlangLong
            OtpErlangMap
            OtpErlangObject
            OtpErlangObject$Hash
            OtpErlangPid
            OtpErlangPort
            OtpErlangRef
            OtpErlangShort
            OtpErlangString
            OtpErlangTuple
            OtpErlangUInt
            OtpErlangUShort]))

(deftest ^:unit atom-constructor-test
  (is (= OtpErlangAtom (type (types/atom true))))
  (is (= OtpErlangAtom (type (types/atom false))))
  (is (= OtpErlangAtom (type (types/atom "some-string")))))

(deftest ^:unit atom-object-protocol-test
  (let [an-atom (types/atom "test-atom")
        same-atom (types/atom "test-atom")
        another-atom (types/atom "another-atom")]
    (is (= "'test-atom'" (types/->str an-atom)))
    (is (= true (types/equal? an-atom an-atom)))
    (is (= true (types/equal? an-atom same-atom)))
    (is (= false (types/equal? an-atom another-atom)))
    (is (= -1226849876 (types/hash an-atom)))))

(deftest ^:unit atom-protocol-test
  (let [an-atom (types/atom "test-atom")
        same-atom (types/atom "test-atom")
        another-atom (types/atom "another-atom")
        true-1 (types/atom true)
        true-2 (types/atom "true")
        false-1 (types/atom false)
        false-2 (types/atom "false")]
    (is (= "'test-atom'" (types/->str an-atom)))
    (is (= true (types/equal? an-atom an-atom)))
    ;; XXX next one isn't working right now
    ;; (is (= "" (types/get-max-length an-atom)))
    (is (= true (types/equal? an-atom same-atom)))
    (is (= false (types/equal? an-atom another-atom)))
    (is (= -1226849876 (types/hash an-atom)))
    (is (= "test-atom" (types/get-atom-value an-atom)))
    (is (= "true" (types/get-atom-value true-1)))
    (is (= "true" (types/get-atom-value true-2)))
    (is (= true (types/get-value true-1)))
    (is (= true (types/get-value true-2)))
    (is (= "false" (types/get-atom-value false-1)))
    (is (= "false" (types/get-atom-value false-2)))
    (is (= false (types/get-value false-1)))
    (is (= false (types/get-value false-2)))))

(deftest ^:unit boolean-protocol-test
  (let [true-bool (types/boolean true)
        false-bool (types/boolean false)]
    (is (= true (types/get-value true-bool)))
    (is (= false (types/get-value false-bool)))))

(deftest ^:unit char-object-protocol-test
  (let [a-char (types/char \A)
        another-char (types/char \B)]
    (is (= "65" (types/->str a-char)))
    (is (= "66" (types/->str another-char)))
    (is (= true (types/equal? a-char a-char)))
    (is (= false (types/equal? a-char another-char)))
    (is (= 65 (types/hash a-char)))
    (is (= 66 (types/hash another-char)))))

(ns clojang.test.jinterface.erlang.types-test
  (:require [clojure.test :refer :all]
            [clojang.jinterface.erlang.types :as types]
            [clojang.jinterface.erlang.atom :as atom-type]
            [clojang.jinterface.erlang.boolean :as boolean-type]
            [clojang.jinterface.erlang.char :as char-type]
            [clojang.jinterface.erlang.tuple :as tuple-type]
            [clojang.jinterface.erlang.list :as list-type]
            [clojang.jinterface.erlang.string :as string-type])
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

(defn setup-java-imports [f]
  (import '[com.ericsson.otp.erlang
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
            OtpErlangUShort])
  (f))

(use-fixtures :once setup-java-imports)

(deftest ^:unit make-erl-name-test
  (is (= 'OtpErlangAtom (types/make-erl-name 'atom))))

(deftest ^:unit init-test
  (is (= OtpErlangAtom (type (types/init 'atom "a")))))

(deftest ^:unit atom-constructor-test
  (is (= OtpErlangAtom (type (types/atom "a"))))
  (is (= OtpErlangAtom (type (types/atom true))))
  (is (= OtpErlangAtom (type (types/atom false)))))

(deftest ^:unit atom-protocol-test
  (let [an-atom (types/atom "test-atom")
        same-atom (types/atom "test-atom")
        another-atom (types/atom "another-atom")
        true-1 (types/atom true)
        true-2 (types/atom "true")
        false-1 (types/atom false)
        false-2 (types/atom "false")]
    (is (= "'test-atom'" (atom-type/->str an-atom)))
    (is (= true (atom-type/equal? an-atom an-atom)))
    ;; XXX next one isn't working right now - it can't find the static field
    ;; (is (= "" (types/get-max-length an-atom)))
    (is (= true (atom-type/equal? an-atom same-atom)))
    (is (= false (atom-type/equal? an-atom another-atom)))
    (is (= -1226849876 (atom-type/hash an-atom)))
    (is (= "test-atom" (atom-type/get-value an-atom)))
    (is (= "true" (atom-type/get-value true-1)))
    (is (= "true" (atom-type/get-value true-2)))
    (is (= true (atom-type/get-boolean-value true-1)))
    (is (= true (atom-type/get-boolean-value true-2)))
    (is (= "false" (atom-type/get-value false-1)))
    (is (= "false" (atom-type/get-value false-2)))
    (is (= false (atom-type/get-boolean-value false-1)))
    (is (= false (atom-type/get-boolean-value false-2)))))

(deftest ^:unit boolean-constructor-test
  (is (= OtpErlangBoolean (type (types/boolean true))))
  (is (= OtpErlangBoolean (type (types/boolean false)))))

(deftest ^:unit boolean-protocol-test
  (let [true-bool (types/boolean true)
        false-bool (types/boolean false)]
    (is (= "true" (boolean-type/get-atom-value true-bool)))
    (is (= "false" (boolean-type/get-atom-value false-bool)))
    (is (= true (boolean-type/get-value true-bool)))
    (is (= false (boolean-type/get-value false-bool)))))

(deftest ^:unit char-constructor-test
  (is (= OtpErlangChar (type (types/char \A)))))

(deftest ^:unit char-protocol-test
  (let [a-char (types/char \A)
        same-char (types/char \A)
        another-char (types/char \B)]
    (is (= "65" (char-type/->str a-char)))
    (is (= "66" (char-type/->str another-char)))
    (is (= true (char-type/equal? a-char same-char)))
    (is (= false (char-type/equal? a-char another-char)))
    (is (= 65 (char-type/hash a-char)))
    (is (= 66 (char-type/hash another-char)))))

(deftest ^:unit tuple-protocol-test
  (let [tuple-1 (types/tuple (into-array [(types/atom "a")]))
        tuple-2 (types/tuple (into-array [(types/atom "a") (types/atom "b")]))
        same-tuple (types/tuple (into-array [(types/atom "a") (types/atom "b")]))]
    (is (= "{a}" (tuple-type/->str tuple-1)))
    (is (= "{a,b}" (tuple-type/->str tuple-2)))
    (is (= true (tuple-type/equal? tuple-2 same-tuple)))
    (is (= false (tuple-type/equal? tuple-1 same-tuple)))
    (is (= -1934275592 (tuple-type/hash tuple-1)))
    (is (= 943796724 (tuple-type/hash tuple-2)))
    (is (= 1 (tuple-type/get-arity tuple-1)))
    (is (= 2 (tuple-type/get-arity tuple-2)))
    (is (= 1 (tuple-type/get-size tuple-1)))
    (is (= 2 (tuple-type/get-size tuple-2)))
    (is (= "a" (atom-type/->str (tuple-type/get-element tuple-1 0))))
    (is (= "b" (atom-type/->str (tuple-type/get-element tuple-2 1))))
    (is (= 1 (count (tuple-type/get-elements tuple-1))))
    (is (= 2 (count (tuple-type/get-elements tuple-2))))))

(deftest ^:unit list-protocol-test
  (let [list-1 (types/list (into-array [(types/atom "a")]))
        list-2 (types/list (into-array [(types/atom "a") (types/atom "b")]))
        same-list (types/list (into-array [(types/atom "a") (types/atom "b")]))]
    (is (= "[a]" (list-type/->str list-1)))
    (is (= "[a,b]" (list-type/->str list-2)))
    (is (= true (list-type/equal? list-2 same-list)))
    (is (= false (list-type/equal? list-1 same-list)))
    (is (= -695209860 (list-type/hash list-1)))
    (is (= -1517993482 (list-type/hash list-2)))
    (is (= 1 (list-type/get-arity list-1)))
    (is (= 2 (list-type/get-arity list-2)))
    (is (= 1 (list-type/get-size list-1)))
    (is (= 2 (list-type/get-size list-2)))
    (is (= 1 (list-type/length list-1)))
    (is (= 2 (list-type/length list-2)))
    (is (= "a" (atom-type/->str (list-type/get-element list-1 0))))
    (is (= "b" (atom-type/->str (list-type/get-element list-2 1))))
    (is (= 1 (count (list-type/get-elements list-1))))
    (is (= 2 (count (list-type/get-elements list-2))))))


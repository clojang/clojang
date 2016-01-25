(ns clojang.test.jinterface.otp.nodes-test
  (:require [clojure.test :refer :all]
            [clojang.jinterface.otp.nodes :as nodes]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang]))

(deftest ^:unit node-constructor-test
  (is (= com.ericsson.otp.erlang.OtpNode
         (type (nodes/node "a"))))
  (is (= com.ericsson.otp.erlang.OtpNode
         (type (nodes/node "a" "cookie"))))
  (is (= com.ericsson.otp.erlang.OtpNode
         (type (nodes/node "a" "cookie" 1234)))))

(deftest ^:unit node-test
  (let [my-name "mynode"
        hostname (util/get-hostname)
        my-fullhost (str my-name "@" hostname)
        my-node (nodes/node my-name)]
    (is (= my-fullhost (nodes/->str my-node)))
    (is (= my-fullhost (nodes/get-name my-node)))
    (is (= hostname (nodes/get-hostname my-node)))
    (is (= my-name (nodes/get-alivename my-node)))
    (is (= String (type (nodes/get-cookie my-node))))
    (is (= Integer (type (nodes/get-port my-node))))
    ;; protected
    ; (is (= "" (nodes/get-epmd my-node)))
    ))

(deftest ^:unit peer-constructor-test
  (is (= com.ericsson.otp.erlang.OtpPeer
         (type (nodes/peer "a")))))

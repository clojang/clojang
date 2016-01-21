(ns clojang.jinterface.otp.nodes-test
  (:require [clojure.test :refer :all]
            [clojang.jinterface.otp.nodes :as nodes]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            AbstractNode
            OtpLocalNode
            OtpNode
            OtpMbox
            OtpPeer
            OtpSelf]))

(deftest ^:unit node-test
  (let [my-name "mynode"
        hostname (util/get-hostname)
        my-fullhost (str my-name "@" hostname)
        my-node (nodes/node my-name)]
    (is (= OtpNode (type my-node)))
    (is (= my-fullhost (nodes/->str my-node)))
    (is (= my-fullhost (nodes/get-name my-node)))
    (is (= hostname (nodes/get-hostname my-node)))
    (is (= my-name (nodes/get-alivename my-node)))
    (is (= String (type (nodes/get-cookie my-node))))
    (is (= Integer (type (nodes/get-port my-node))))
    ;; protected
    ; (is (= "" (nodes/get-epmd my-node)))
    ))


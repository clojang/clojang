(ns clojang.types.record)

(defrecord Pid
  [creation
   node
   id
   serial])

(defrecord Msg
  [msg
   msg-type
   recipient
   recipient-name
   recipient-pid
   sender-pid])

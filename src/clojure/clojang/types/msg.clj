(ns clojang.types.msg)

(defrecord Msg
  [msg
   msg-type
   recipient
   recipient-name
   recipient-pid
   sender-pid])

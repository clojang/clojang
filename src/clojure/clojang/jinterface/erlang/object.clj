(ns clojang.jinterface.erlang.object
  (:import [com.ericsson.otp.erlang OtpErlangObject])
  (:refer-clojure :exclude [hash]))

(defprotocol ErlangObject
  (bind [this binds]
    "Make new Erlang term replacing variables with the respective values
    from bindings argument(s).")
  (clone [this]
    "Clone the Erlang object.")
  (decode [this buff]
    "Read binary data in the Erlang external format, and produce a
    corresponding Erlang data type object.")
  (encode [this buff]
    "Convert the object according to the rules of the Erlang external
    format.")
  (equal? [this other-erl-obj]
    "Determine if two Erlang objects are equal.")
  (hash [this]
    "Get the object hash code.")
  (match [this term binds]
    "Perform match operation against given term.")
  (->str [this]
    "Convert to a string."))

(def object-behaviour
  {:bind (fn [this binds] (.bind this binds))
   :clone (fn [this] (.clone this))
   :decode (fn [this buff] (.decode this buff))
   :encode (fn [this buff] (.encode this buff))
   :equal? (fn [this other-erl-obj] (.equals this other-erl-obj))
   :hash (fn [this] (.hashCode this))
   :match (fn [this term binds] (.match this term binds))
   :->str (fn [this] (.toString this))})

(extend OtpErlangObject ErlangObject object-behaviour)

(ns clojang.jinterface.otp.messaging
  {:lang :core.typed}
  (:require [clojure.core.typed :as t :refer [IFn ann ann-protocol]]
            [clojure.core.typed.utils :refer [defprotocol]]
            [clojang.jinterface.otp :as otp]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            OtpErlangObject
            OtpErlangPid
            OtpInputStream
            OtpMbox
            OtpMsg
            OtpNode])
  (:refer-clojure :exclude [hash send defprotocol]))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; OTP constructors
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(ann mbox [OtpNode -> (t/Nilable OtpMbox)])
(defn mbox
  "A wrapper for the mbox-creation method on nodes. See
  ``clojang.jinterface.otp.nodes/node``."
  [^OtpNode node-instance]
  (.createMbox node-instance))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; OTP protocols
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(ann-protocol MboxObject
  close
  [MboxObject -> nil]
  equal?
  [MboxObject Object -> boolean]
  exit
  (t/IFn [MboxObject OtpErlangObject -> nil]
         ;; XXX In Java, reason can be a string.
         #_[MboxObject String -> nil]
         [MboxObject OtpErlangPid OtpErlangObject -> nil]
         ;; XXX In Java, reason can be a string.
         #_[MboxObject OtpErlangPid String -> nil])
  get-name
  [MboxObject -> (t/Nilable String)]
  get-names
  [MboxObject -> (Array3 String (t/Nilable String) (t/Nilable String))]
  hash
  [MboxObject -> int]
  link
  [MboxObject OtpErlangPid -> nil]
  ping
  (t/IFn [MboxObject String -> boolean]
         [MboxObject String long -> boolean])
  receive
  (t/IFn [MboxObject -> (t/Nilable OtpErlangObject)]
         [MboxObject long -> (t/Nilable OtpErlangObject)])
  receive-buf
  (t/IFn [MboxObject -> (t/Nilable OtpInputStream)]
         [MboxObject long -> (t/Nilable OtpInputStream)])
  receive-msg
  (t/IFn [MboxObject -> (t/Nilable OtpMsg)]
         [MboxObject long -> (t/Nilable OtpMsg)])
  register-name
  [MboxObject String -> t/Bool]
  self
  [MboxObject -> (t/Nilable OtpErlangPid)]
  get-pid
  [MboxObject -> (t/Nilable OtpErlangPid)]
  send
  (t/IFn [MboxObject OtpErlangPid OtpErlangObject -> nil]
         ;; XXX Figure out how to accept a node name
         #_[MboxObject String OtpErlangObject -> nil]
         [MboxObject String String OtpErlangObject -> nil])
  !
  (t/IFn [MboxObject OtpErlangPid OtpErlangObject -> nil]
         ;; XXX Figure out how to accept a node name
         #_[MboxObject String OtpErlangObject -> nil]
         [MboxObject String String OtpErlangObject -> nil])
  unlink
  [MboxObject OtpErlangPid -> nil]
  whereis
  [MboxObject String -> (t/Nilable OtpErlangPid)])
(defprotocol MboxObject
  "Provides a simple mechanism for exchanging messages with Erlang processes
  or other instances of this class.

  Each mailbox is associated with a unique pid that contains information
  necessary for delivery of messages. When sending messages to named
  processes or mailboxes, the sender pid is made available to the recipient
  of the message. When sending messages to other mailboxes, the recipient
  can only respond if the sender includes the pid as part of the message
  contents. The sender can determine its own pid by calling ``(self sndr)``

  Mailboxes can be named, either at creation or later. Messages can be sent
  to named mailboxes and named Erlang processes without knowing the pid
  that identifies the mailbox. This is neccessary in order to set up initial
  communication between parts of an application. Each mailbox can have at
  most one name.

  Since this class was intended for communication with Erlang, all of the
  send methods take OtpErlangObject arguments. However this class can also
  be used to transmit arbitrary Java objects (as long as they implement one
  of ``java.io.Serializable`` or ``java.io.Externalizable``) by
  encapsulating the object in a OtpErlangBinary.

  Messages to remote nodes are externalized for transmission, and as a
  result the recipient receives a copy of the original Java object. To
  ensure consistent behaviour when messages are sent between local
  mailboxes, such messages are cloned before delivery.

  Additionally, mailboxes can be linked in much the same way as Erlang
  processes. If a link is active when a mailbox is closed, any linked
  Erlang processes or OtpMboxes will be sent an exit signal. As well, exit
  signals will be (eventually) sent if a mailbox goes out of scope and its
  finalize() method called. However due to the nature of finalization (i.e.
  Java makes no guarantees about when finalize() will be called) it is
  recommended that you always explicitly close mailboxes if you are using
  links *in*nstead of relying on finalization to notify other parties in a
  timely manner.

  When retrieving messages from a mailbox that has received an exit signal,
  an OtpErlangExit exception will be raised. Note that the exception is queued
  in the mailbox along with other messages, and will not be raised until it
  reaches the head of the queue and is about to be retrieved."
  (close [this]
    "Close the given mailbox.")
  (equal? [this other-obj]
    "Determine if two mailboxes are equal.")
  (exit [this reason] [this recip-pid reason]
    "Close the given mailbox with a given reason or send an exit signal to
    a remote pid.")
  (get-name [this]
    "Get the registered name of this mailbox.")
  (get-names [this]
    "Get a list of all known registered names on the same node as this
    mailbox.")
  (hash [this]
    "Get the object hash code.")
  (link [this]
    "Link to a remote mailbox or Erlang process.")
  (ping [this node-name] [this node-name timeout]
    "Create a connection to a remote node.")
  (receive [this] [this timeout]
    "Block until a message (Erlang object) arrives for this mailbox, or if
    a timeout is given, wait for a message until the timeout has been
    reached.")
  (receive-buf [this] [this timeout]
    "Block until a message (Erlang input stream) arrives for this mailbox, or
    if a timeout is given, wait for a message until the timeout has been
    reached.")
  (receive-msg [this] [this timeout]
    "Block until a message (OTP message) arrives for this mailbox, or
    if a timeout is given, wait for a message until the timeout has been
    reached.")
  (register-name [this mbox-name]
    "Register or remove a name for this mailbox.")
  (self [this]
    "Get the identifying ``pid`` associated with the given mailbox.")
  (get-pid [this]
    "Alias for ``self``.")
  (send [this recip-pid msg] [this mbox-name node-name msg]
    "Send a message to a remote ``pid``, representing either another mailbox
    or an Erlang process or to a remote node by mailbox name and node name.")
  (! [this recip-pid msg] [this mbox-name node-name msg]
    "An alias for ``send``")
  (unlink [this recip-pid]
    "Remove a link to a remote mailbox or Erlang process.")
  (whereis [this mbox-name]
    "Determine the pid corresponding to a registered name on this node."))

(extend-type OtpMbox
  MboxObject
  (close [this]
    (.close this))
  (equal? [this other-obj]
    (.equals this other-obj))
  (exit
    ([this reason]
     (.exit this ^OtpErlangObject reason))
    ([this ^OtpErlangPid recip-pid ^OtpErlangObject reason]
     (.exit this recip-pid reason)))
  (get-name [this]
    (.getName this))
  (get-names [this]
    (.getNames this))
  (hash [this]
    (.hashCode this))
  (link [this recip-pid]
    (.link this recip-pid))
  ;; XXX put the ping with the default timeout in the mid-level API
  (ping
    ([this node-name]
     (.ping this node-name 1000))
    ([this node-name timeout]
     (.ping this node-name timeout)))
  (receive
    ([this]
     (.receive this))
    ([this timeout]
     (.receive this timeout)))
  (receive-buf
    ([this]
     (.receiveBuf this))
    ([this timeout]
     (.receiveBuf this timeout)))
  (receive-msg
    ([this]
     (.receiveMsg this))
    ([this timeout]
     (.receiveMsg this timeout)))
  (register-name [this mbox-name]
    (.registerName this mbox-name))
  (self [this]
    (.self this))
  (get-pid [this]
    (.self this))
  (send
    ([this ^OtpErlangPid recip-pid ^OtpErlangObject msg]
     (.send this recip-pid msg))
    ([this mbox-name node-name msg]
     (.send this mbox-name node-name msg)))
  (!
    ([this recip-pid msg]
     (send this recip-pid msg))
    ([this mbox-name node-name msg]
     (send this mbox-name node-name msg)))
  (unlink [this recip-pid]
    (.unlink this recip-pid))
  (whereis [this mbox-name]
    (.whereis this mbox-name)))

(ann-protocol MsgObject
  get-msg
  [MsgObject -> (t/Nilable OtpErlangObject)]
  get-recipient
  [MsgObject -> (t/Nilable Object)]
  get-recipient-name
  [MsgObject -> (t/Nilable String)]
  get-recipient-pid
  [MsgObject -> (t/Nilable OtpErlangPid)]
  get-sender-pid
  [MsgObject -> (t/Nilable OtpErlangPid)]
  get-type
  [MsgObject -> int])
(defprotocol MsgObject
  "Provides a carrier for Erlang messages.

  Instances of this class are created to package header and payload information
  in received Erlang messages so that the recipient can obtain both parts with
  a single call to receiveMsg().

  The header information that is available is as follows:

  * a tag indicating the type of message
  * the intended recipient of the message, either as a pid or as a String, but
    never both.
  * (sometimes) the sender of the message. Due to some eccentric
    characteristics of the Erlang distribution protocol, not all messages have
    information about the sending process. In particular, only messages whose
    tag is regSendTag contain sender information.

  Message are sent using the Erlang external format (see separate
  documentation). When a message is received and delivered to the recipient
  mailbox, the body of the message is still in this external representation
  until ``get-msg`` is called, at which point the message is decoded. A copy of
  the decoded message is stored in the OtpMsg so that subsequent calls to
  ``get-msg`` do not require that the message be decoded a second time."
  (get-msg [this]
    "Deserialize and return a new copy of the message contained in this
    ``OtpMsg``.")
  (get-recipient [this]
    "Get the name of the recipient for this message, if it is a ``regSendTag``
    message.")
  (get-recipient-name [this]
    "Get the name of the recipient for this message.")
  (get-recipient-pid [this]
    "Get the Pid of the recipient for this message, if it is a ``sendTag``
    message.")
  (get-sender-pid [this]
    "Get the Pid of the sender of this message.")
  (get-type [this]
    "Get the type marker from this message."))

(extend-type OtpMsg
  MsgObject
  (get-msg [this]
    (.getMsg this))
  (get-recipient [this]
    (.getRecipient this))
  (get-recipient-name [this]
    (.getRecipientName this))
  (get-recipient-pid [this]
    (.getRecipientPid this))
  (get-sender-pid [this]
    (.getSenderPid this))
  (get-type [this]
    (.type this)))

(def msg-type-lookup
  {OtpMsg/exit2Tag   :exit-2
   OtpMsg/exitTag    :exit
   OtpMsg/linkTag    :link
   OtpMsg/regSendTag :reg-send
   OtpMsg/sendTag    :send
   OtpMsg/unlinkTag  :unlink})

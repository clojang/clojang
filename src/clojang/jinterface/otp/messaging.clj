(ns clojang.jinterface.otp.messaging
  (:require [clojang.jinterface.otp :as otp]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            OtpMbox])
  (:refer-clojure :exclude [hash send]))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; OTP constructors
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defn mbox
  "A wrapper for the mbox-creation method on nodes."
  [node-instance]
  (.createMbox node-instance))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; OTP protocols
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
    "Determine if two Erlang objects are equal.")
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
  (send [this recip-pid msg] [this mbox-name node-name msg]
    "Send a message to a remote ``pid``, representing either another mailbox
    or an Erlang process or to a remote node by mailbox name and node name.")
  (! [this recip-pid msg] [this mbox-name node-name msg]
    "An alias for ``send``")
  (unlink [this recip-pid]
    "Remove a link to a remote mailbox or Erlang process.")
  (whereis [this node-name]
    "Determine the pid corresponding to a registered name on this node."))

(extend-type OtpMbox MboxObject
  (close [this]
    (.close this))
  (equal? [this other-obj]
    (.equals this other-obj))
  (exit
    ([this reason]
      (.exit this))
    ([this recip-pid reason]
      (.exit recip-pid reason)))
  (get-name [this]
    (.getName this))
  (get-names [this]
    (.getNames this))
  (hash [this]
    (.hashCode this))
  (link [this recip-pid]
    (.link this recip-pid))
  (ping [this node-name timeout]
    (.toString this))
  (ping [this node-name]
    (.toString this 3000))
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
  (send
    ([this recip-pid-or-name msg]
      (.send this recip-pid-or-name msg))
    ([this mbox-name node-name msg]
      (.send this mbox-name node-name msg)))
  (!
    ([this recip-pid-or-name msg]
      (.send this recip-pid-or-name msg))
    ([this mbox-name node-name msg]
      (.send this mbox-name node-name msg)))
  (unlink [this recip-pid]
    (.unlink this recip-pid))
  (whereis [this mbox-name]
    (.whereis this mbox-name)))

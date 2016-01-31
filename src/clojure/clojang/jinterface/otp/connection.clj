(ns clojang.jinterface.otp.connection
  (:require [clojang.jinterface.otp :as otp]
            [clojang.util :as util])
  (:import [com.ericsson.otp.erlang
            AbstractConnection
            OtpConnection
            OtpCookedConnection])
  (:refer-clojure :exclude [deliver send]))

(defprotocol AbstractConnectionObject
  "Maintains a connection between a Java process and a remote Erlang, Java or C
  node. The object maintains connection state and allows data to be sent to and
  received from the peer.

  This abstract class provides the neccesary methods to maintain the actual
  connection and encode the messages and headers in the proper format according
  to the Erlang distribution protocol. Subclasses can use these methods to
  provide a more or less transparent communication channel as desired.

  Note that no receive methods are provided. Subclasses must provide methods
  for message delivery, and may implement their own receive methods.

  If an exception occurs in any of the methods in this class, the connection
  will be closed and must be reopened in order to resume communication with the
  peer. This will be indicated to the subclass by passing the exception to its
  ``deliver`` function.

  The ``System`` property ``OtpConnection.trace`` can be used to change the
  initial trace level setting for all connections. Normally the initial trace
  level is 0 and connections are not traced unless ``set-trace-level`` is used
  to change the setting for a particular connection. ``OtpConnection.trace``
  can be used to turn on tracing by default for all connections."
  (close [this]
    "Close the connection to the remote node.")
  (deliver [this msg-or-exception]
    "Deliver messages or communication exceptions to the recipient.")
  (get-flags [this]
    "")
  (get-trace-level [this]
    "Get the trace level for this connection.")
  (connected? [this]
    "Determine if the connection is still alive.")
  (run [this]
    "")
  (set-flags [this flag-integer]
    "")
  (set-trace-level [this level-integer]
    "Set the trace level for this connection."))

(def abstract-connection-behaviour
  {:close (fn [this] (.close this))
   :deliver (fn [this msg-or-exception] (.deliver this msg-or-exception))
   :get-flags (fn [this] (.getFlags this))
   :get-trace-level (fn [this] (.getTraceLevel this))
   :connected? (fn [this] (.isConnected this))
   :run (fn [this] (.run this))
   :set-flags (fn [this flag-integer] (.setFlags this flag-integer))
   :set-trace-level (fn [this level-integer] (.setTraceLevel this level-integer))})

(extend AbstractConnection
        AbstractConnectionObject
        abstract-connection-behaviour)

(defprotocol ConnectionObject
  "Maintains a connection between a Java process and a remote Erlang, Java or C
  node. The object maintains connection state and allows data to be sent to and
  received from the peer.

  Once a connection is established between the local node and a remote node,
  the connection object can be used to send and receive messages between the
  nodes and make rpc calls (assuming that the remote node is a real Erlang
  node).

  The various receive methods are all blocking and will return only when a
  valid message has been received or an exception is raised.

  If an exception occurs in any of the methods in this class, the connection
  will be closed and must be explicitely reopened in order to resume
  communication with the peer.

  It is not possible to create an instance of this class directly.
  ``OtpConnection`` objects are returned by ``OtpSelf.connect()`` and
  ``OtpSelf.accept()``."
  (exit [this dest-pid reason]
    "Send an exit signal to a remote process.")
  (link [this dest-pid]
    "Create a link between the local node and the specified process on the
    remote node.")
  (get-msg-count [this]
    "Return the number of messages currently waiting in the receive queue for
    this connection.")
  (get-peer [this]
    "Get information about the node at the peer end of this connection.")
  (receive [this] [this timeout]
    "Receive a message from a remote process.")
  (receive-buf [this] [this timeout]
    "Receive a raw (still encoded) message from a remote process.")
  (receive-msg [this] [this timeout]
    "Receive a messge complete with sender and recipient information.")
  (receive-rpc [this]
    "Receive an RPC reply from the remote Erlang node.")
  (get-self [this]
    "Get information about the node at the local end of this connection.")
  (send [this dest msg]
    "Send a message to a process on a remote node.")
  (send-buf [this desg msg]
    "Send a pre-encoded message to a process on a remote node.")
  (send-rpc [this mod fun args]
    "Send an RPC request to the remote Erlang node.")
  (unlink [this dest-pid]
    "Remove a link between the local node and the specified process on the
    remote node."))

(def connection-behaviour
   {:exit (fn [this dest-pid reason] (.exit this dest-pid reason))
    :link (fn [this dest-pid] (.link this dest-pid))
    :get-msg-count (fn [this] (.msgCount this))
    :get-peer (fn [this] (.peer this))
    :receive (fn ([this] (.receive this))
                 ([this timeout] (.receive this timeout)))
    :receive-buf (fn ([this] (.receiveBuf this))
                     ([this timeout] (.receiveBuf this timeout)))
    :receive-msg (fn ([this] (.receiveMsg this))
                     ([this timeout] (.receiveMsg this timeout)))
    :receive-rpc (fn ([this] (.receiveRPC this))
                     ([this timeout] (.receiveRPC this timeout)))
    :get-self (fn [this] (.self this))
    :send (fn [this dest msg] (.send this dest msg))
    :send-buf (fn [this desg msg] (.sendBuf this desg msg))
    :send-rpc (fn [this mod fun args] (.sendRPC this mod fun args))
    :unlink (fn [this dest-pid] (.unlink this dest-pid))})

(extend OtpConnection
        ConnectionObject
        connection-behaviour)

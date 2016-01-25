# Clojang User's Guide: The Mid-level API

**NOTICE**: This document is a copy of the *Clojang User's Guide: The Low-level API*, but with significant changes made. Unlike the low-level guide, this one provides information for developers interested in using the Clojang API which is designed to save time and effort without the need of manually translating between Clojure and JInterface Erlang data types.

Where usage is identical to the low-level API guide, related content is not duplicated in this guide (with the exception of code and text necessary for establishing a clear context).


## Nodes

A node as defined by Erlang/OTP is an instance of the Erlang Runtime System, a virtual machine roughly equivalent to a JVM. Each node has a unique name in the form of an identifier composed partly of the hostname on which the node is running, e.g ``gurka@sallad.com``. In this example, the host name is appended automatically to the identifier, and the port number is chosen by the underlying system:

```clojure
=> (require '[clojang.core :as clojang])
nil
=> (def node (clojang/node "gurka"))
#'user/node
```


## Mailboxes

Erlang processes running on an Erlang node are identified by process identifiers (pids) and, optionally, by registered names unique within the node. Each Erlang process has an implicit mailbox that is used to receive messages; the mailbox is identified with the pid of the process.

JInterface provides a similar mechanism with the class [OtpMbox](erlang/java/com/ericsson/otp/erlang/OtpMbox.html), a mailbox that can be used to send and receive messages asynchronously. Each OtpMbox is identified with a unique pid and , optionally, a registered name unique within the [OtpMbox](erlang/java/com/ericsson/otp/erlang/OtpMbox.html).

Applications are free to create mailboxes as necessary. This is done as follows:

```clojure
user=> (def mbox (clojang/create-mbox node))
#'user/mbox
```

The mailbox created in the above example has no registered name, although it does have a pid. The pid can be obtained from the mailbox and included in messages sent from the mailbox, so that remote processes are able to respond.

An application can register a name for a mailbox, either when the mailbox is initially created:

```clojure
user=> (def mbox (clojang/create-mbox node "server"))
#'user/mbox
```

or later on, if need be. You may either use the ``register-mbox`` function for the Node (takes three arguments):

```clojure
=> (clojang/register-mbox node "server2" mbox)
true
```

or the ``register-name`` function for the Mbox:

```clojure
=> (clojang/register-name mbox "server3")
true
```

Registered names are usually necessary in order to start communication, since it is impossible to know in advance the pid of a remote process. If a well-known name for one of the processes is chosen in advance and known by all communicating parties within an application, each mailbox can send an initial message to the named mailbox, which then can identify the sender pid.


##  Connections

It is not necessary to explicitly set up communication with a remote node. Simply sending a message to a mailbox on that node will cause the OtpNode to create a connection if one does not already exist. Once the connection is established, subsequent messages to the same node will reuse the same connection.

It is possible to check for the existence of a remote node before attempting to communicate with it. Here we send a ping message to the remote node to see if it is alive and accepting connections. Paste the following function in your REPL:

```clojure
(defn print-liveliness [node other]
  (if (clojang/ping node other 1000)
    (println "It's aliiiive!")
    (println "Mate, this node wouldn't go 'voom' if ...")))
```

Now let's use it:

```clojure
user=> (print-liveliness node "gurka")
It's aliiiive!
nil
user=> (print-liveliness node "nohost")
Mate, this node wouldn't go 'voom' if ...
nil
```

If the call to ``(nodes/ping ...)`` succeeds, a connection to the remote node has been established. Note that it is not necessary to ping remote nodes before communicating with them, but by using ping you can determine if the remote exists before attempting to communicate with it.

Connections are only permitted by nodes using the same security cookie. The cookie is a short string provided either as an argument when creating [node](clojang/current/clojang.jinterface.otp.nodes.html#var-NodeObject) objects, or found in the user's home directory in the file ``.erlang.cookie``. When a connection attempt is made, the string is used as part of the authentication process. If you are having trouble getting communication to work, use the trace facility (described later in this document) to show the connection establishment. A likely problem is that the cookies are different.

## Sending and Receiving Messages

Messages sent with this package must be instances of [object](clojang/current/clojang.jinterface.erlang.object.html) or one of its subclasses. Message can be sent to processes or pids, either by specifying the pid of the remote, or its registered name and node.

In this example, we create a message containing our own pid so the echo process can reply:

```clojure
TBD
```

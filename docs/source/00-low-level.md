# Clojang User's Guide: The Low-level API

## The JInterface Package in Clojang

**NOTICE**: This document is a copy of the JInterface Users Guide, [a version of which](erlang/jinterface_users_guide.html) is provided in the published documentation for the Clojang project.

The [Jinterface](erlang/java/com/ericsson/otp/erlang/package-summary.html) package provides a set of tools for communication with Erlang processes. It can also be used for communication with other Java processes using the same package, as well as C processes using the Erl_Interface library.

The set of classes in the package can be divided into two categories: those that provide the actual communication, and those that provide a Java representation of the Erlang data types. The latter are all subclasses of OtpErlangObject, and they are identified by the ``OtpErlang`` prefix.

Since this package provides a mechanism for communicating with Erlang, message recipients can be Erlang processes or instances of com.ericsson.otp.erlang.OtpMbox, both of which are identified with pids and possibly registered names. When pids or mailboxes are mentioned as message senders or recipients in this section, it should assumed that even Erlang processes are included, unless specified otherwise. The classes in [Jinterface](erlang/java/com/ericsson/otp/erlang/package-summary.html) support the following:


*    manipulation of data represented as Erlang data types
*    conversion of data between Java and Erlang formats
*    encoding and decoding of Erlang data types for transmission or storage
*    communication between Java nodes and Erlang processes

In the following sections, these topics are described:

*    mapping of Erlang types to Java
*    encoding, decoding, and sending Erlang terms
*    connecting to a distributed Erlang node
*    using nodes, mailboxes and EPMD
*    sending and receiving Erlang messages and data
*    remote procedure calls
*    linking to remote processes
*    compiling your code for use with Jinterface
*    tracing message flow

http://oubiwann.github.io/clojang/current/clojang.jinterface.erlang.types.html#var-atom

## Mapping of Basic Erlang Types to the JVM

This section describes the mapping of Erlang basic types to JVM types.

| Erlang type         | JVM type
|---------------------|------------------------------------------------------
|atom                 | [atom](clojang/current/clojang.jinterface.erlang.atom.html)
|binary               | [OtpErlangBinary](erlang/java/com/ericsson/otp/erlang/OtpErlangBinary.html)
|floating point types | [OtpErlangFloat](erlang/java/com/ericsson/otp/erlang/OtpErlangFloat.html) or [OtpErlangDouble](erlang/java/com/ericsson/otp/erlang/OtpErlangDouble.html), depending on the floating point value size
|integral types       | One of [OtpErlangByte](erlang/java/com/ericsson/otp/erlang/OtpErlangByte.html), [char](clojang.jinterface.erlang.types.html#var-charhtml),[OtpErlangShort](erlang/java/com/ericsson/otp/erlang/OtpErlangShort.html), [OtpErlangUShort](erlang/java/com/ericsson/otp/erlang/OtpErlangUShort.html),[OtpErlangInt](erlang/java/com/ericsson/otp/erlang/OtpErlangInt.html),[OtpErlangUInt](erlang/java/com/ericsson/otp/erlang/OtpErlangUInt.html) or [OtpErlangLong](erlang/java/com/ericsson/otp/erlang/OtpErlangLong.html), depending on the integral value size and sign
|list                 | [list](clojang/current/clojang.jinterface.erlang.list.html)
|pid                  | [OtpErlangPid](erlang/java/com/ericsson/otp/erlang/OtpErlangPid.html)
|port                 | [OtpErlangPort](erlang/java/com/ericsson/otp/erlang/OtpErlangPort.html)
|ref                  | [OtpErlangRef](erlang/java/com/ericsson/otp/erlang/OtpErlangRef.html)
|tuple                | [tuple](clojang.jinterface.erlang.types.html#var-tuple.html)
|map                  | [OtpErlangMap](erlang/java/com/ericsson/otp/erlang/OtpErlangMap.html)
|term                 | [OtpErlangObject](clojang/current/clojang.jinterface.erlang.object.html)


## Special Mapping Issues

The atoms ``true`` and ``false`` are special atoms, used as boolean values. The class [boolean](clojang/current/clojang.jinterface.erlang.boolean.html) can be used to represent these.

Lists in Erlang are also used to describe sequences of printable characters (strings). A convenience class [string](clojang/current/clojang.jinterface.erlang.string.html) is provided to represent Erlang strings.


## Nodes

A node as defined by Erlang/OTP is an instance of the Erlang Runtime System, a virtual machine roughly equivalent to a JVM. Each node has a unique name in the form of an identifier composed partly of the hostname on which the node is running, e.g ``gurka@sallad.com``. Several such nodes can run on the same host as long as their names are unique. The class [node](clojang/current/clojang.jinterface.otp.nodes.html#var-NodeObject) represents an Erlang node. It is created with a name and optionally a port number on which it listens for incoming connections. Before creating an instance of [node](clojang/current/clojang.jinterface.otp.nodes.html#var-NodeObject), ensure that EPMD is running on the host machine. See the Erlang documentation for more information about EPMD. In this example, the host name is appended automatically to the identifier, and the port number is chosen by the underlying system:

```clojure
=> (require '[clojang.jinterface.otp.nodes :as nodes])
nil
=> (def node (nodes/node "gurka"))
#'user/node
```


## Mailboxes

Erlang processes running on an Erlang node are identified by process identifiers (pids) and, optionally, by registered names unique within the node. Each Erlang process has an implicit mailbox that is used to receive messages; the mailbox is identified with the pid of the process.

JInterface provides a similar mechanism with the class [OtpMbox](erlang/java/com/ericsson/otp/erlang/OtpMbox.html), a mailbox that can be used to send and receive messages asynchronously. Each OtpMbox is identified with a unique pid and , optionally, a registered name unique within the [OtpMbox](erlang/java/com/ericsson/otp/erlang/OtpMbox.html).

Applications are free to create mailboxes as necessary. This is done as follows:

```clojure
user=> (def mbox (nodes/create-mbox node))
#'user/mbox
```

The mailbox created in the above example has no registered name, although it does have a pid. The pid can be obtained from the mailbox and included in messages sent from the mailbox, so that remote processes are able to respond.

An application can register a name for a mailbox, either when the mailbox is initially created:

```clojure
user=> (def mbox (nodes/create-mbox node "server"))
#'user/mbox
```

or later on, if need be. You may either use the ``register-mbox`` function for the Node):

```clojure
=> (nodes/register-mbox node "server2" mbox)
true
```

or the ``register-name`` function for the Mbox:

```clojure
=> (require '[clojang.jinterface.otp.messaging :as messaging])
nill
=> (messaging/register-name mbox "server3")
true
```

Registered names are usually necessary in order to start communication, since it is impossible to know in advance the pid of a remote process. If a well-known name for one of the processes is chosen in advance and known by all communicating parties within an application, each mailbox can send an initial message to the named mailbox, which then can identify the sender pid.


##  Connections

It is not necessary to explicitly set up communication with a remote node. Simply sending a message to a mailbox on that node will cause the OtpNode to create a connection if one does not already exist. Once the connection is established, subsequent messages to the same node will reuse the same connection.

It is possible to check for the existence of a remote node before attempting to communicate with it. Here we send a ping message to the remote node to see if it is alive and accepting connections. Paste the following function in your REPL:

```clojure
(defn print-liveliness [node other]
  (if (nodes/ping node other 1000)
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

Connections are never broken explicitly. If a node fails or is closed, a connection may be broken however.


##  Transport Factory

All necessary connections are made using methods of [OtpTransportFactory](erlang/java/com/ericsson/otp/erlang/OtpTransportFactory.html) interface. Default OtpTransportFactory implementation is based on standard Socket class. User may provide custom transport factory as needed. See java doc for details.


## Sending and Receiving Messages

Messages sent with this package must be instances of [object](clojang/current/clojang.jinterface.erlang.object.html) or one of its subclasses. Message can be sent to processes or pids, either by specifying the pid of the remote, or its registered name and node.

In this example, we create a message containing our own pid so the echo process can reply:

```clojure
TBD
```

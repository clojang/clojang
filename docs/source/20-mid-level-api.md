# Clojang User's Guide: The Mid-level API

**NOTICE**: This document is a copy of the *Clojang User's Guide: The Low-level API*, but with significant changes made. Unlike the low-level guide, this one provides information for developers interested in using the Clojang API which is designed to save time and effort without the need of manually translating between Clojure and JInterface Erlang data types.

Where usage is identical to the low-level API guide, related content is not duplicated in this guide (with the exception of code and text necessary for establishing a clear context).


## Nodes

A node as defined by Erlang/OTP is an instance of the Erlang Runtime System, a virtual machine roughly equivalent to a JVM. Each node has a unique name in the form of an identifier composed partly of the hostname on which the node is running, e.g ``gurka@sallad.com``. In this example, the host name is appended automatically to the identifier, and the port number is chosen by the underlying system:

```clojure
=> (require '[clojang.conn :as conn]
            '[clojang.core :as clojang]
            '[clojang.mbox :as mbox]
            '[clojang.node :as node])
nil
=> (def gurka (node/new :gurka))
#'user/gurka
```


## Mailboxes

Erlang processes running on an Erlang node are identified by process identifiers (pids) and, optionally, by registered names unique within the node. Each Erlang process has an implicit mailbox that is used to receive messages; the mailbox is identified with the pid of the process.

Clojang provides a similar mechanism with the namespace [clojang.mbox](clojang/current/clojang.mbox.html), a mailbox that can be used to send and receive messages asynchronously. Each OtpMbox is identified with a unique pid and , optionally, a registered name unique within the [OtpMbox](erlang/java/com/ericsson/otp/erlang/OtpMbox.html).

Applications are free to create mailboxes as necessary. This is done as follows, optionally giving it a registered name:

```clojure
user=> (def inbox (mbox/new gurka :echo))
#'user/mbox
```

Registered names are usually necessary in order to start communication, since it is impossible to know in advance the pid of a remote process. If a well-known name for one of the processes is chosen in advance and known by all communicating parties within an application, each mailbox can send an initial message to the named mailbox, which then can identify the sender pid.


##  Connections

It is not necessary to explicitly set up communication with a remote node. Simply sending a message to a mailbox on that node will cause the OtpNode to create a connection if one does not already exist. Once the connection is established, subsequent messages to the same node will reuse the same connection.

It is possible to check for the existence of a remote node before attempting to communicate with it. Here we send a ping message to the remote node to see if it is alive and accepting connections. Paste the following function in your REPL:

```clojure
(defn print-liveliness [this-node other-node]
  (if (node/ping this-node other-node 1000)
    (println "It's aliiiive!")
    (println "Mate, this node wouldn't go 'voom' if ...")))
```

Now let's use it:

```clojure
user=> (print-liveliness gurka :gurka)
It's aliiiive!
nil
user=> (print-liveliness gurka :nohost)
Mate, this node wouldn't go 'voom' if ...
nil
```

If the call to ``(nodes/ping ...)`` succeeds, a connection to the remote node has been established. Note that it is not necessary to ping remote nodes before communicating with them, but by using ping you can determine if the remote exists before attempting to communicate with it.

Connections are only permitted by nodes using the same security cookie. The cookie is a short string provided either as an argument when creating [node](clojang/current/clojang.jinterface.otp.nodes.html#var-NodeObject) objects, or found in the user's home directory in the file ``.erlang.cookie``. When a connection attempt is made, the string is used as part of the authentication process. If you are having trouble getting communication to work, use the trace facility (described later in this document) to show the connection establishment. A likely problem is that the cookies are different.

## Sending and Receiving Messages

Messages sent with this package must be instances of [object](clojang/current/clojang.jinterface.erlang.object.html) or one of its subclasses. Message can be sent to processes or pids, either by specifying the pid of the remote, or its registered name and node.

In this example, we create a message containing our own pid so the echo process can reply:

```clojure
=> (def msg [(mbox/get-pid inbox) :hello-world])
#'user/msg
=> (mbox/! inbox :echo :gurka msg)
nil
=> (mbox/receive inbox)
[#object[com.ericsson.otp.erlang.OtpErlangPid
         0x1fe20514
         "#Pid<gurka@mndltl01.1.0>"]
 :hello-world]
```

You can also send messages from Erlang VMs to your ``node``'s mailbox named ``"echo"``. Before you do that, though, start listening in your Clojure REPL:

```clojure
=> (mbox/receive inbox)
```

Next, start up LFE (Lisp Flavoured Erlang) on the same machine with a short name:

```bash
$ /path/to/bin/lfe -sname lfe
LFE Shell V7.2 (abort with ^G)
(lfe@mndltl01)>
```

Once you're in the REPL, you're ready to send a message:

```cl
(lfe@mndltl01)> (! #(echo gurka@mndltl01) #(hej!))
#(hej!)
```

Looking at the Clojure REPL, you'll see that your ``receive `` call has finished and you now have some data:

```clojure
[:hej!]
```


##  Sending Arbitrary Data

TBD


## Linking to Remote Processes

TBD


##  Using EPMD

TBD


## Remote Procedure Calls

An Erlang node acting as a client to another Erlang node typically sends a request and waits for a reply. Such a request is included in a function call at a remote node and is called a remote procedure call. Remote procedure calls are supported through the [clojang.conn]() namespace. The following example shows how the ``connection`` protocol is used for remote procedure calls:

```clojure
(def self (node/self :client))
(def other (node/peer "clojang-lfe@mndltl01"))
(def connx (node/connect self other))

(conn/send-rpc connx :erlang :date)
(conn/receive-rpc connx)
[2016 1 30]
```

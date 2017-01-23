# Clojang User's Guide

**NOTICE**: This document is an adaptatoin of the *jiface User's Guide*,
(which, in turn was copied from the
[JInterface User's Guide](erlang/jinterface_users_guide.html)); the
adaptations include significant changes, due to a much more simplified API
(or, perhaps more accurately, a more varied API but with sane defaults
provided). Unlike the low-level guide upon which it is based, this one
provides information for developers interested in using the Clojang API which
is designed to save time and effort without the need of manually translating
between Clojure and JInterface's Erlang data types.

In the following examples, we assume that you have `require`ed the necessary
namespaces and `import`ed the Java classes needed in a Clojang REPL:

```clj
=> (require '[clojang.conn :as conn]
            '[clojang.core :as clojang :refer [! receive self]]
            '[clojang.mbox :as mbox]
            '[clojang.node :as node])
=> (import '[com.ericsson.otp.erlang OtpErlangDecodeException
                                     OtpErlangExit])
```


## Nodes

A node as defined by Erlang/OTP is an instance of the Erlang Runtime System, a
virtual machine roughly equivalent to a JVM. Each node has a unique name in
the form of an identifier composed partly of the hostname on which the node is
running, e.g ``gurka@sallad.com``.

When starting an Erlang VM in distribution mode, a node process is started,
one with its own message box and the ability to send and receive messages.
JInterface by itself does no such thing when the JVM is started. However,
Clojang makes use of a Java agent (written in Clojure) to provide this same
facility. As such, a Clojure REPL running Clojang has its own default node and
associated message box, just like an Erlang or LFE node:

```clj
=> (node/get-default)
#object[com.ericsson.otp.erlang.OtpNode ...]
```

It is recommended that you use the default node created for you by the Clojang
agent, in order to reduce overhead and the memory footprint of Clojang
applications. However, the API provides the ability to create your own nodes,
should you wish to.

To do so, you may either use a short name or a name including the host:

```clj
=> (def gurka (node/new :gurka))
#'user/gurka
=> (def gurka (node/new "gurka@localhost"))
#'user/gurka
```


## Mailboxes

Erlang processes running on an Erlang node are identified by process
identifiers (pids) and, optionally, by registered names unique within the
node. Each Erlang process has an implicit mailbox that is used to receive
messages; the mailbox is identified with the pid of the process.

Clojang provides a similar mechanism with the namespace
[clojang.mbox](clojang/current/clojang.mbox.html),
a mailbox that can be used to send and receive messages asynchronously. Each
`OtpMbox` is identified with a unique pid and , optionally, a registered name
unique within the
[OtpMbox](erlang/java/com/ericsson/otp/erlang/OtpMbox.html).

When the Clojang agent creates a default node for use by Clojang in a Java VM,
it also creates a default message inbox for that node. This is accessible via
the following function call:

```clj
=>(mbox/get-default)
#object[com.ericsson.otp.erlang.OtpMbox ...]
```

Applications are free to create mailboxes as necessary. This is done as
follows, optionally giving it a registered name:

```clj
user=> (def inbox (mbox/new gurka :echo))
#'user/mbox
```

Registered names are usually necessary in order to start communication, since
it is impossible to know in advance the pid of a remote process. If a well-
known name for one of the processes is chosen in advance and known by all
communicating parties within an application, each mailbox can send an initial
message to the named mailbox, which then can identify the sender pid.


##  Connections

It is not necessary to explicitly set up communication with a remote node.
Simply sending a message to a mailbox on that node will cause the OtpNode to
create a connection if one does not already exist. Once the connection is
established, subsequent messages to the same node will reuse the same
connection.

It is possible to check for the existence of a remote node before attempting
to communicate with it. Here we send a ping message to the remote node to see
if it is alive and accepting connections. Paste the following function in your
REPL:

```clj
(defn print-liveliness [node-name]
  (case (node/ping node-name)
    :pong (println "It's aliiiive!")
    :pang (println "Mate, this node wouldn't go 'voom' if ...")))
```

Now let's use it:

```clj
user=> (print-liveliness :gurka)
It's aliiiive!
nil
user=> (print-liveliness :nohost)
Mate, this node wouldn't go 'voom' if ...
nil
```

If the call to `(node/ping ...)` succeeds, a connection to the remote node
has been established. Note that it is not necessary to ping remote nodes
before communicating with them, but by using ping you can determine if the
remote exists before attempting to communicate with it.

Connections are only permitted by nodes using the same security cookie. The
cookie is a short string provided either as an argument when creating
[node](clojang/current/clojang.jinterface.otp.nodes.html#var-NodeObject)
objects, or found in the user's home directory in the file `.erlang.cookie`.
When a connection attempt is made, the string is used as part of the
authentication process. If you are having trouble getting communication to
work, use the trace facility (described later in this document) to show the
connection establishment. A likely problem is that the cookies are different.


## Sending and Receiving Messages

Messages sent with this package must be instances of
[object](clojang/current/clojang.jinterface.erlang.object.html) or one of its
subclasses. Message can be sent to processes or pids, either by specifying the
pid of the remote, or its registered name and node.

In this example, we create a message containing our own pid so the echo
process can reply:

```clj
=> (def msg [(mbox/get-pid inbox) :hello-world])
#'user/msg
=> (! inbox :echo :gurka msg)
:ok
=> (receive inbox)
[#clojang.types.Pid{:creation 2, :node "gurka@localhost", :id 1, :serial 0}
 :hello-world]
```

You can also send messages from Erlang VMs to your `node`'s mailbox named
`"echo"`. Before you do that, though, start listening in your Clojure REPL:

```clj
=> (receive inbox)
```

Next, start up LFE (Lisp Flavoured Erlang) on the same machine with a short
name:

```bash
$ /path/to/bin/lfe -sname lfe
LFE Shell V7.2 (abort with ^G)
(lfe@host)>
```

Once you're in the REPL, you're ready to send a message:

```cl
(lfe@host)> (! #(echo gurka@host) #(hej!))
#(hej!)
```

Looking at the Clojure REPL, you'll see that your `receive` call has
finished and you now have some data:

```clj
[:hej!]
```


##  Sending Arbitrary Data

This package was originally intended to be used for communicating between Java
and Erlang, and for that reason the send and receive methods all use Java
representations of Erlang data types.

However it is possible to use the package to communicate with remote processes
written in Java as well, and in these cases it may be desirable to send other
data types.

The simplest way to do this is to encapsulate arbitrary data in messages of
type OtpErlangBinary. The OtpErlangBinary class can be created from arbitrary
Java objects that implement the Serializable or Externalizable interface:

```clj
TBD
```


## Linking to Remote Processes

Erlang defines a concept known as linked processes. A link is an implicit
connection between two processes that causes an exception to be raised in one
of the processes if the other process terminates for any reason. Links are
bidirectional: it does not matter which of the two processes created the link
or which of the linked processes eventually terminates; an exception will be
raised in the remaining process. Links are also idempotent: at most one link
can exist between two given processes, only one operation is necessary to
remove the link.

`clojang` provides a similar mechanism. Also here, no distinction is made
between mailboxes and Erlang processes. A link can be created to a remote
mailbox or process when its pid is known:

```clj
(mbox/link (mbox/get-pid inbox))
```

The link can be removed by either of the processes in a similar manner:

```clj
(mbox/unlink (mbox/get-pid inbox))
```

In the cases when only a "remote" message box is provided (as above), the
local node in the `link` and `unlink` function calls is assumed to be the
default node.

If the remote process terminates while the link is still in place, an
error will be returned on a subsequent call to `receive`. For example, in
this case, the "remote" node's inbox pid to which we have linked is the
`OtpMbox` instance stored in the `inbox` variable. The local node is our
default node. Before unlinking `inbox`, if we instead call
`(mbox/close inbox)` and then try to receive on the local node's default
message box, we'll get an error. Here's one way to handle that error:

```clj
(match (receive)
  [:error [_ msg _]]
    (println (format "Remote pid %s has terminated."
                     (exceptions/get-pid ex)))
  data data)
```


##  Using EPMD

`epmd` is the Erlang Port Mapper Daemon. Distributed Erlang nodes register with
`epmd` on the localhost to indicate to other nodes that they exist and can
accept connections. `epmd` maintains a register of node and port number
information, and when a node wishes to connect to another node, it first
contacts epmd in order to find out the correct port number to connect to.

The basic interaction with EPMD is done through the functions in the
`clojang.epmd` namespace.  Under the hood (at the JInterface level), nodes
wishing to contact other nodes  first request information from `epmd` before a
connection can be set up.

When manually creating connections to Erlang nodes with operations such as
`(node/connect (self) ...)`, a connection is
first made to `epmd` and, if the node is known, a connection is then made to
the Erlang node.

Clojang nodes can also register themselves with `epmd` if they want other
nodes in the system to be able to find and connect to them. This is done by
call  to `(epmd/publish-port ...)`.

Be aware that on some systems (such as VxWorks), a failed node will not be
detected by this mechanism since the operating system does not automatically
close descriptors that were left open when the node failed. If a node has
failed in this way, `epmd` will prevent you from registering a new node with
the old name, since it thinks that the old name is still in use. In this case,
you must unregister the name explicitly, by using `(epmd/unpublish-port ...)`.
This will cause `epmd` to close the connection from the far end. Note that if
the name was in fact still in use by a node, the results of this operation are
unpredictable. Also, doing this does not cause the local end of the connection
to close, so resources may be consumed.


## Remote Procedure Calls

An Erlang node acting as a client to another Erlang node typically sends a
request and waits for a reply. Such a request is included in a function call
at a remote node and is called a remote procedure call. Remote procedure calls
are supported through the
[clojang.rpc](http://clojang.github.io/clojang/current/clojang.rpc.html)
namespace. The following example shows how the `rpc` protocol is used for
remote procedure calls:

```clj
(rpc/! :clojang-lfe :erlang :date)
(rpc/receive :clojang-lfe)
[2016 1 30]
```


## Results

### Functions with Return Values

In `clojang`, functions that return values (many of which obtain them from
calls to `jiface`) often need to convert from the underlying `JInterface`
custom Java types to Clojure types and or data structures. This is done
whenever possible, providing as native a feel of the `clojang` API as
possible.


### Functions with Side-effects

In Erlang, functions that do not return results sometimes return `true`,
other times `ok` or `{ok}` (the latter being an Erlang tuple type). In Clojang,
these are all unified under a single return value of `:ok`.


## Errors and Exception-handling

Clojang whole-heartedly adopts the Erlang ethos of dealing with errors as data.
As such the only time you will see an exception in Clojang is if:

* you are using code which is still under development
* you have discovered a bug

All functions that result in an underlying Java, `JInterface`, or Clojure
exception will instead return a vector of the following form:

```clj
[:error [exception-type exception-message exception]]
```

You may use this to great advantage (clarity and conciseness) in your code with
pattern matching, similarly as to how these are handled in Erlang:

```clj
TBD
```

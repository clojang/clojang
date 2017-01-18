# Talking to Servers: LFE & Clojure

This tutorial covers the two-sided topic of communications between LFE and
Clojure:

* Clojure communicating as a client with LFE servers, and
* LFE communicating as a client with Clojure servers


## Clojure Client with LFE Server

Creating robust servers is the bread and butter of LFE/OTP, so we'll start
with Clojure as a client and LFE taking the role of running some servers.
We'll look at two examples:

1. A very simple LFE server, and
1. An LFE ``gen_server`` built with OTP


### A Simple LFE Server

Before we look at the "right way" to start thinking about long-running
processes in the Erlang VM (using the infrastructure of OTP to build for
reliability), we're going to try out a highly simplified LFE server ... of the
"ping-pong" variety.

First, start up an LFE REPL by running this command in the cloned `clojang`
directory:

```bash
$ make repl
LFE Shell V7.2 (abort with ^G)
(clojang-lfe@host)>
```

This will put you at an LFE prompt showing the node name and the host,
indicating that you are running in "distributed" mode, able to communicate
with other nodes:

```cl
(clojang-lfe@host)>
```

Paste the following into the REPL at the prompt:

```cl
(defun ping-pong (count)
  (receive
    (`#(ping ,caller)
      (! caller 'pong) (ping-pong (+ 1 count)))
    (`#(get-count ,caller)
      (! caller count) (ping-pong count))
    (`#(stop ,caller)
      (! caller 'stopped) 'stopped)))

(defun start-server (init-state)
  (spawn
    (lambda ()
      (ping-pong init-state))))
```

Keep in mind that this is for demonstration purposes only! The simplicity of
the above example may inspire you, but you'll need more than this to run your
LFE/Elixir/Erlang apps in production!

All we need now to turn this into a simplistic server is to spawn it:

```cl
(clojang-lfe@host)> (set ping-pong-pid (start-server 0))
<0.71.0>
```

With the LFE REPL as our implicit client, let's send the server some messages:

```cl
(clojang-lfe@host)> (! ping-pong-pid `#(ping ,(self)))
#(ping <0.68.0>)
(clojang-lfe@host)> (! ping-pong-pid `#(ping ,(self)))
#(ping <0.68.0>)
(clojang-lfe@host)> (! ping-pong-pid `#(ping ,(self)))
#(ping <0.68.0>)
```

Since we're using the LFE REPL as the client, we'll need to `flush` its
inbox to get the messages that our simple little server is sending to it:

```cl
(clojang-lfe@host)> (c:flush)
Shell got pong
Shell got pong
Shell got pong
ok
```

Let's make the other call:

```cl
(clojang-lfe@host)> (! ping-pong-pid `#(get-count ,(self)))
#(get-count <0.68.0>)
(clojang-lfe@host)> (c:flush)
Shell got 3
ok
```

We're going to want to call this from Clojure too, so let's register the LFE
process with a name:

```cl
(clojang-lfe@host)> (register 'ping-pong ping-pong-pid)
true
```

In a separate terminal window (in the Clojang top-level directory, just like
the `make repl` command), start up the Clojure REPL:

```bash
$ lein repl
nREPL server started on port 58369 on host 127.0.0.1 - nrepl://127.0.0.1:58369
REPL-y 0.3.7, nREPL 0.2.10
Clojure 1.8.0
clojang.dev=>
```

The Clojang project automatically loads up a development namespace for you,
`clojang.dev`, when you start the Clojure REPL, with everything you need to
start talking to Erlang nodes.

With the `clojang.dev` REPL running, w're ready to try out some calls to our
simple LFE server:

```clj
clojang.dev=> (! :ping-pong "clojang-lfe@host" [:ping (self)])
:ok
clojang.dev=> (! :ping-pong "clojang-lfe@host" [:ping (self)])
:ok
clojang.dev=> (! :ping-pong "clojang-lfe@host" [:ping (self)])
:ok
clojang.dev=> (receive)
:pong
clojang.dev=> (receive)
:pong
clojang.dev=> (receive)
:pong
clojang.dev=> (! :ping-pong "clojang-lfe@host" [:get-count (self)])
:ok
clojang.dev=> (receive)
6
clojang.dev=> (! :ping-pong "clojang-lfe@host" [:stop (self)])
:ok
clojang.dev=> (receive)
:stopped

```

Back on the LFE side, we can check to make sure that the process was indeed
stopped:

```cl
(clojang-lfe@host)> (is_process_alive ping-pong-pid)
false
```

To make sure our node no longer has an open port, we can query EPMD:

```clj
clojang.dev=> (conn/lookup-names)
["name clojang-lfe at port 52346"]
```


### Using OTP

Now we're going to look at creating an LFE server that you could put into a
supervision tree and run in production: LFE with OTP.


#### LFE-side Server

In the `examples` directory of the Clojang source code there is a module
containing a variation on the classic "ping-pong" server. It's written in LFE,
but it could have been done in any BEAM (Erlang VM) language. Here's the code:

```cl
(defmodule ping-pong
  (behaviour gen_server)
  (export
    ;; gen_server implementation
    (start 0)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
    ;; server API
    (ping 0)
    (get-ping-count 0)))

;;; config functions

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))

;;; gen_server implementation

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;;; callback implementation

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_call
  (('ping _caller state-data)
    `#(reply pong ,(+ 1 state-data)))
  (('ping-count _caller state-data)
    `#(reply ,state-data ,state-data))
  (('stop _caller state-data)
    `#(stop shutdown ok ,state-data))
  ((_message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; our server API

(defun ping ()
  (gen_server:call (server-name) 'ping))

(defun get-ping-count ()
  (gen_server:call (server-name) 'ping-count))
```

We're going to compile and then run that code from the LFE REPL. Then, from a
Clojure REPL, we'll talk to it.

From the previous example, you should already have your LFE REPL running.
Let's go ahead and compile the example:

```cl
(clojang-lfe@host)> (c "examples/ping-pong.lfe")
(#(module ping-pong))
```

Now we can start it:

```cl
(clojang-lfe@host)> (ping-pong:start)
#(ok <0.45.0>)
```


#### Clojure-side RPC

With our LFE server running, let's jump back over to the Clojure REPL and make
some RPC calls to our LFE server by passing the module `:ping-pong` and the
function name `:ping`:

```clj
clojang.dev=> (rpc/! :clojang-lfe :ping-pong :ping)
:ok
clojang.dev=> (rpc/! :clojang-lfe :ping-pong :ping)
:ok
clojang.dev=> (rpc/! :clojang-lfe :ping-pong :ping)
:ok
clojang.dev=> (rpc/receive :clojang-lfe)
:pong
clojang.dev=> (rpc/receive :clojang-lfe)
:pong
clojang.dev=> (rpc/receive :clojang-lfe)
:pong
clojang.dev=> (rpc/! :clojang-lfe :ping-pong :get-ping-count)
:ok
clojang.dev=> (rpc/receive :clojang-lfe)
3
```

You may also use the fully qualified node name, if you wish -- but you must
choose one or the other, not both. If you wish to switch, you will need to
close the implicit connection created first:

```clj
clojang.dev=> (rpc/close :clojang-lfe)
:ok
```

Now you can use a different node name (which will reconnect to the remote
node):

```clj
clojang.dev=> (rpc/! "clojang-lfe@host" :ping-pong :ping)
:ok
clojang.dev=> (rpc/receive "clojang-lfe@host")
:pong
clojang.dev=> (rpc/receive "clojang-lfe@host" :ping-pong :get-ping-count)
:ok
clojang.dev=> (rpc/receive "clojang-lfe@host")
4
clojang.dev=> (rpc/close "clojang-lfe@host")
:ok
```

While the emphasis here is remote communications, it goes without saying that
local LFE sends are also possible. Since this is a `gen_server`
implementation, the proper way to do this is by calling the API we defined:

```cl
(clojang-lfe@host)> (ping-pong:ping)
pong
(clojang-lfe@host)> (ping-pong:ping)
pong
(clojang-lfe@host)> (ping-pong:ping)
pong
(clojang-lfe@host)> (ping-pong:get-ping-count)
7
```

However, a remote LFE node would use the Erlang `rpc` module similarly to how
we did from Clojure:

```cl
(clojang-lfe@host)> (rpc:cast 'clojang-lfe@host 'ping-pong 'ping '())
true
(clojang-lfe@host)> (rpc:call 'clojang-lfe@host 'ping-pong 'get-ping-count '())
8
```

Note that in the `rpc` Erlang module, `cast` is used for when no result is
expected and `call` is used when there is.

The `clojang.rpc` namespace provides `cast` and `call` as convenience
functions which perform a `send` and then `receive` in the same function,
emulating a blocking call:

```clj
clojang.dev=> (rpc/cast :clojang-lfe :ping-pong :ping)
:ok
clojang.dev=> (rpc/call :clojang-lfe :ping-pong :get-ping-count)
9
```

You may, of course, use `call` with the remote `ping` function, since it does
return a value:

```clj
clojang.dev=> (rpc/call :clojang-lfe :ping-pong :ping)
:pong
```

Our example use of RPC has focused strictly on the module we compiled (and its
contained functions). Regardless, the same usage applies to all Erlang modules
and functions on the remote node, however:

```clj
clojang.dev=> (rpc/call :clojang-lfe :erlang :date)
[2017 1 17]
clojang.dev=> (rpc/call :clojang-lfe :erlang :abs [-1])
1
```

Let's cleanup:

```clj
clojang.dev=> (rpc/close :clojang-lfe)
:ok
```


## LFE Client with Clojure Server

Just as with the LFE server examples above, we'll write a simple Clojure
server first, communicate with it via an LFE client, and then write a bit more
robust example Clojure server.


### Simple Clojure Server

With the core.match library for Clojure, we are able to get remarkably close to the
little server we wrote in LFE above:

```clojure
(require '[clojure.core.match :refer [match]])

(defn ping-pong
  []
  (let [init-state 0
        self (node/new :srvr)
        inbox (mbox/new self :pinger)]
    (loop [png-count init-state]
      (match [(mbox/receive inbox)]
        [[:ping caller]]
          (do (mbox/! inbox caller :pong)
              (recur (inc png-count)))
        [[:get-count caller]]
          (do (mbox/! inbox caller png-count)
              (recur png-count))
        [[:stop caller]]
          (do (mbox/close inbox)
              (node/close self)
              :stopped)))))
```

Not only are both LFE and Clojure Lisps (though LFE is a Lisp-2 ... and then
some ... while Clojure is a Lisp-1), but with the addition of core.match to
the Clojure code, we can have a very similar developer experience in both
languages. How nice!

Let's paste this server into the Clojure REPL and then run it:

```clojure
(ping-pong)
```

Now let's head over to an LFE REPL and talk to the Clojure server:

```cl
(clojang-lfe@host)> (! #(pinger srvr@host) `#(ping ,(self)))
#(ping <0.34.0>)
(clojang-lfe@host)> (! #(pinger srvr@host) `#(ping ,(self)))
#(ping <0.34.0>)
(clojang-lfe@host)> (! #(pinger srvr@host) `#(ping ,(self)))
#(ping <0.34.0>)
(clojang-lfe@host)> (c:flush)
Shell got pong
Shell got pong
Shell got pong
ok
(clojang-lfe@host)> (! #(pinger srvr@host) `#(get-count ,(self)))
#(get-count <0.34.0>)
(clojang-lfe@host)> (c:flush)
Shell got 3
ok
```

Once we're done, we can ask the server to stop from LFE:

```cl
(clojang-lfe@host)> (! #(pinger srvr@host) `#(stop ,(self)))
#(stop <0.34.0>)
```

Back in the Clojure REPL you should now see:

```clojure
:stopped
```


### XXX [Do something with the text below]

In the case of an LFE server, Erlang/OTP defined the service specification
(i.e., `gen_server`), but there is no analog in Clojure (core, anyway; the
[Pulsar](http://docs.paralleluniverse.co/pulsar/#behaviors) library provides
an OTP-inspired `gen-server`).

Further complicating matters, JInterface uses Java threads when creating
connections between nodes; any misbehaving nodes, mailboxes, or issues with
the communications between the two can result in various errors (expected and
otherwise) in the VM that is running the Clojure/JInterface code.

As such, the more explicitly we deal with threads and their related issues in
Clojure, the more likely we are to have a properly running Clojure+JInterface
server. As such, using a particular framework in addition to this, could very
well mask any issues that might arise with threads and JInterface, and unless
you have a great deal of experience running threaded networking code in your
Clojure (or other JVM language) framework of choice, we recommend not
complicating matters.

Note that an ideal solution would take advantage of any Erlang-term generating
or parsing code in JInterface, while leaving the message passing to a library
built upon core.async. This would provide Clojure programmers with two highly
performant microthread sysmtems (core.async and the Erlang VM) which
specialize in concurrency. Alas, that day has not yet arrived ...


### Creating a JInterface Server

Since there is no framework built around JInterface nodes, mailboxes, and
connections, we'll be working the threads that JInterface creates, taking
basic defensive measures (not the best approach, but programming with threads
doesn't leave us much choice).

Sticking with the RPC example, there is no OTP-compliant mechanism for making
calls on remote nodes in Clojure. This, of course, is no surprise, since the
OTP RPC mechanism is very specific to the Erlang VM.

That being said, it's "simply" sturctured message passing, and there's no
reason we cannot implement our own RPC server -- we just need to be able to
hanle RPC messages. The Clojang library offers just this, and in fact, will
automatically parse RPC-type messages sent from an Erlang VM node (in our
case, LFE, but the mechanism is dialect-agnostic).

To demonstate this we need to set up a long-running Clojure XXX (analog to
Erlang VM "process", dependent upon implementation) and then make requests to
it. The Clojure XXX needs to accept OTP messages, parse them, evaluate the
specified code, and then send the results back to the LFE node.

We use the same example as above: a simple ping-pong server which accepts RPC
calls. Here is the source code:

```clojure
TBD
```

To run the server in the Clojure REPL, slurp it and XXX:

```clojure
TBD
```

Now, from your LFE REPL, make some calls:

```cl
TBD
```

As with the previous example, it's possible to call the ping-pong server from
the same node where the server is running:

```clojure
TBD
```


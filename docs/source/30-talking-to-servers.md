# Talking to Servers: LFE & Clojure

## Clojure Client with LFE Server

### Using OTP

In the ``examples`` directory of the Clojang source code there is a module containing a variation on the classic "ping-pong" server. It's written in LFE, but it could have been done in any BEAM (Erlang VM) language. Here's the code:

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

We're going to compile and then run that code from the LFE REPL. Then, from a Clojure REPL, we'll talk to it.

Start the LFE REPL using the ``repl`` make target in the Clojang top-level directory:

```bash
$ make repl
LFE Shell V7.2 (abort with ^G)
(clojang-lfe@mndltl01)>
```

Next, compile the example:

```cl
(clojang-lfe@mndltl01)> (c "examples/ping-pong.lfe")
(#(module ping-pong))
```

Now we can start it:

```cl
(clojang-lfe@mndltl01)> (ping-pong:start)
#(ok <0.45.0>)
```

In a separate terminal window (in the Clojang top-level directory, just like the ``make repl`` command), start up the Clojure REPL:

```bash
$ lein repl
nREPL server started on port 58369 on host 127.0.0.1 - nrepl://127.0.0.1:58369
REPL-y 0.3.7, nREPL 0.2.10
Clojure 1.8.0
clojang.dev=>
```

Now we can create nodes for two ends of the connection, and then open a connection between the two:

```clojure
clojang.dev=> (def self (node/self :client))
#'clojang.dev/self
clojang.dev=> (def other (node/peer "clojang-lfe@mndltl01"))
#'clojang.dev/other
clojang.dev=> (def connx (node/connect self other))
#'clojang.dev/connx
```

With a connection established, we're able to execute Erlang module/function calls on the remote LFE/Erlang node:

```clojure
clojang.dev=> (conn/!rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/!rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/!rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/!rpc connx :ping-pong :get-ping-count)
nil
clojang.dev=> (conn/receive-rpc connx)
3
```

While the emphasis here is remote communications, it goes without saying that local sends are also possible. Since this is a ``gen_server`` implementation, the proper way to do this is by calling the API we defined:

```cl
(clojang-lfe@mndltl01)> (ping-pong:ping)
pong
(clojang-lfe@mndltl01)> (ping-pong:ping)
pong
(clojang-lfe@mndltl01)> (ping-pong:ping)
pong
(clojang-lfe@mndltl01)> (ping-pong:get-ping-count)
6
```


### Quick and Dirty

What we've demonstrated above is the "right way" to start thinking about long-running processes in the Erlang VM: using the infrastructure of OTP to build for reliability. We could have written something similar, but much more simply. It would also have been very fragile.

For instance, we could have done it like this:

```cl
(clojang-lfe@mndltl01)> (defun png-png (count)
                          (receive
                            (`#(ping ,caller)
                              (! caller 'pong) (png-png (+ 1 count)))
                            (`#(get-count ,caller)
                              (! caller count) (png-png count))
                            (`#(stop, caller)
                              (! caller 'stopped) 'stopped)))
png-png
```

Please don't write your LFE OTP apps like this, though :-) You'll slowly end-up reinventing OTP, though a partially-implemented and bug-ridden one ...

Back to this demonstration: all we need now to turn this into a simplistic server is to spawn it:

```cl
(clojang-lfe@mndltl01)> (set png-png-pid (spawn (lambda () (png-png 0))))
<0.71.0>
```

Let's test it out:

```cl
(clojang-lfe@mndltl01)> (! png-png-pid `#(ping ,(self)))
#(ping <0.68.0>)
(clojang-lfe@mndltl01)> (! png-png-pid `#(ping ,(self)))
#(ping <0.68.0>)
(clojang-lfe@mndltl01)> (! png-png-pid `#(ping ,(self)))
#(ping <0.68.0>)
(clojang-lfe@mndltl01)> (c:flush)
Shell got pong
Shell got pong
Shell got pong
ok
```

Since we're using the LFE REPL as the client, we'll need to ``flush`` its inbox to get the messages that our simple little server is sending to it.

Let's make the other call:

```cl
(clojang-lfe@mndltl01)> (! png-png-pid `#(get-count ,(self)))
#(get-count <0.68.0>)
(clojang-lfe@mndltl01)> (c:flush)
Shell got 3
ok
```

We're going to want to call this from Clojure too, so let's register the LFE process with a name:

```cl
(clojang-lfe@mndltl01)> (register 'png-png png-png-pid)
true
```

With that done, let's return to the Clojure REPL and make some calls to our second (simple-don't-deploy-with-this) server:

```clojure
clojang.dev=> (conn/! connx :png-png [:ping (node/get-pid self)])
nil
clojang.dev=> (conn/! connx :png-png [:ping (node/get-pid self)])
nil
clojang.dev=> (conn/! connx :png-png [:ping (node/get-pid self)])
nil
clojang.dev=> (conn/receive connx)
:pong
clojang.dev=> (conn/receive connx)
:pong
clojang.dev=> (conn/receive connx)
:pong
clojang.dev=> (conn/! connx :png-png [:get-count (node/get-pid self)])
nil
clojang.dev=> (conn/receive connx)
6
```

That looks not-so-erily familiar ...


## LFE Client with Clojure Server

In the case of an LFE server, Erlang/OTP defined the service specification (i.e., ``gen_server``), but there is no analog in Clojure (core, anyway; the [Pulsar](http://docs.paralleluniverse.co/pulsar/#behaviors) library provides an OTP-inspired ``gen-server``).

Further complicating matters, JInterface uses Java threads when creating connections between nodes; any misbehaving nodes, mailboxes, or issues with the communications between the two can result in various errors (expected and otherwise) in the VM that is running the Clojure/JInterface code.

As such, the more explicitly we deal with threads and their related issues in Clojure, the more likely we are to have a properly running Clojure+JInterface server. As such, using a particular framework in addition to this, could very well mask any issues that might arise with threads and JInterface, and unless you have a great deal of experience running threaded networking code in your Clojure (or other JVM language) framework of choice, we recommend not complicating matters.

Note that an ideal solution would take advantage of any Erlang-term generating or parsing code in JInterface, while leaving the message passing to a library built upon core.async. This would provide Clojure programmers with two highly performant microthread sysmtems (core.async and the Erlang VM) which specialize in concurrency. Alas, that day has not yet arrived ...

### Creating a JInterface Server

Since there is no framework built around JInterface nodes, mailboxes, and connections, we'll be working the threads that JInterface creates, taking basic defensive measures (not the best approach, but programming with threads doesn't leave us much choice).

Sticking with the RPC example, there is no OTP-compliant mechanism for making calls on remote nodes in Clojure. This, of course, is no surprise, since the OTP RPC mechanism is very specific to the Erlang VM.

That being said, it's "simply" sturctured message passing, and there's no reason we cannot implement our own RPC server -- we just need to be able to hanle RPC messages. The Clojang library offers just this, and in fact, will automatically parse RPC-type messages sent from an Erlang VM node (in our case, LFE, but the mechanism is dialect-agnostic).

To demonstate this we need to set up a long-running Clojure XXX (analog to Erlang VM "process", dependent upon implementation) and then make requests to it. The Clojure XXX needs to accept OTP messages, parse them, evaluate the specified code, and then send the results back to the LFE node.

We use the same example as above: a simple ping-pong server which accepts RPC calls. Here is the source code:

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

As with the previous example, it's possible to call the ping-pong server from the same node where the server is running:

```clojure
TBD
```

### Quick and Dirty

With the core.match library for Clojure, we are able to get remarkably close to the
little server we wrote in LFE above:

```clojure
(require '[clojure.core.match :refer [match]])

(defn png-png
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

Let's paste this server into the Clojure REPL and then run it:

```clojure
(png-png)
```

Now let's head over to an LFE REPL and talk to the Clojure server:

```cl
(clojang-lfe@mndltl01)> (! #(pinger srvr@mndltl01) `#(ping ,(self)))
#(ping <0.34.0>)
(clojang-lfe@mndltl01)> (! #(pinger srvr@mndltl01) `#(ping ,(self)))
#(ping <0.34.0>)
(clojang-lfe@mndltl01)> (! #(pinger srvr@mndltl01) `#(ping ,(self)))
#(ping <0.34.0>)
(clojang-lfe@mndltl01)> (c:flush)
Shell got pong
Shell got pong
Shell got pong
ok
(clojang-lfe@mndltl01)> (! #(pinger srvr@mndltl01) `#(get-count ,(self)))
#(get-count <0.34.0>)
(clojang-lfe@mndltl01)> (c:flush)
Shell got 3
ok
```

Once we're done, we can ask the server to stop from LFE:

```cl
(clojang-lfe@mndltl01)> (! #(pinger srvr@mndltl01) `#(stop ,(self)))
#(stop <0.34.0>)
```

Back in the Clojure REPL you should now see:

```clojure
:stopped
```

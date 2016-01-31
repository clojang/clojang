# Talking to Servers

## Clojure Client with LFE Server

In the ``examples`` directory of the Clojang source code there is a module containing a variation on the classic "ping-pong" server. It's written in LFE, but it could have been done in any BEAM (Erlang VM) language. We're going to compile and then run that code from the LFE REPL.

Then, from a Clojure REPL, we'll talk to it.

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
#(ok <0.51.0>)
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

With a connection established, we're able to execute Erlang module/function calls on the remove LFE/Erlang node:

```clojure
clojang.dev=> (conn/send-rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/send-rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/send-rpc connx :ping-pong :ping)
nil
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/receive-rpc connx)
:pong
clojang.dev=> (conn/send-rpc connx :ping-pong :get-ping-count)

```

## LFE Client with Clojure Server

TBD

# Talking to Servers

## Clojure Client with LFE Server

In the examples directory of the Clojang source code there is an example LFE server. We're going to compile and then run that code from the LFE REPL, then from a Clojure REPL we'll talk to it.

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

In a separate terminal window (also in the Clojang top-level directory), start up the Clojure REPL:

```bash
$ lein repl
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

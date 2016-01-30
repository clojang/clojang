# clojang

[![][clojang-logo]][clojang-logo-large]

[clojang-logo]: resources/images/clojang-logo-250x.png
[clojang-logo-large]: resources/images/clojang-logo-1000x.png

*Erlang's JInterface in Idiomatic Clojure*

![Clojars Project](http://clojars.org/clojang/latest-version.svg)


#### Table of Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Building](#building-)
* [Shells & REPLs](#shells---repls-)
* [Documentation](#documentation-)
* [Usage](#usage-)
  * [Low-level API](#low-level-api-)
  * [Mid-level API](#mid-level-api-)
  * [Running Tests](#running-tests-)
* [Erlang, Clojure, and JInterface](#erlang-clojure-and-jinterface-)
* [License](#license-)


## Introduction [&#x219F;](#table-of-contents)

TBD


## Dependencies [&#x219F;](#table-of-contents)

* Java
* Erlang
* lein
* rebar3


## Building [&#x219F;](#table-of-contents)

``rebar3`` is used for the top-level builds of the project, running ``lein`` under the covers in order to build the Clojure code and ``.jar`` file. To build everything -- LFE, Erlang, and Clojure -- you need only do the following:

* ``rebar3 compile``

If you wish to build your own JInterface ``.jar`` file and not use the one we've uploaded to Clojars, you'll need to follow the instrucations given in the documentation here:

* [Building JInterface for Clojure](http://oubiwann.github.io/clojang/current/05-building-jinterface.html)


## Shells & REPLs [&#x219F;](#table-of-contents)

There are three interactive programming environments you may start, each of which will have full access to the project's libraries/dependencies.

LFE:

```bash
$ make repl
```

Erlang:

```bash
$ rebar3 shell
```

```
Clojure:
$ lein repl
```

## Documentation [&#x219F;](#table-of-contents)

Project documentation, including Clojang API reference docs, Javadocs for JInterface, and the Erlang JInterface User's Guide, is availble here:

* [http://oubiwann.github.io/clojang/current/](http://oubiwann.github.io/clojang/current/)

Quick links for the other docs:

* Clojang User Guides:
  * [Low-level API](http://oubiwann.github.io/clojang/current/10-low-level.html) - A translation of the *JInterface User's Guide* (Erlang documantaion) from Java into Clojure
  * [Mid-level API](http://oubiwann.github.io/clojang/current/20-mid-level.html) - An adaptation of the *Low-level API User's Guide* for even more idiomatic Clojure use
* [JInterface User's Guide](http://oubiwann.github.io/clojang/current/erlang/jinterface_users_guide.html)
* [Jinterface Javadocs](http://oubiwann.github.io/clojang/current/erlang/java)


## Usage [&#x219F;](#table-of-contents)

The documentation section provides links to developer guides and API references, but below are also provided two quick examples, one each in the low- and mid-level APIs.

### Low-level API [&#x219F;](#table-of-contents)

```clojure
(require '[clojang.jinterface.otp.messaging :as messaging]
         '[clojang.jinterface.otp.nodes :as nodes]
         '[clojang.jinterface.erlang.types :as types]
         '[clojang.jinterface.erlang.tuple :as tuple-type])
(def node (nodes/node "gurka"))
(def mbox (messaging/mbox node))
(messaging/register-name mbox "echo")
(def msg (into-array
           (types/object)
           [(messaging/self mbox)
            (types/atom "hello, world")]))
(messaging/! mbox "echo" "gurka" (types/tuple msg))
(messaging/receive mbox)
#object[com.ericsson.otp.erlang.OtpErlangTuple
        0x4c9e3fa6
        "{#Pid<gurka@mndltl01.1.0>,'hello, world'}"]
```

From LFE:

```cl
(lfe@mndltl01)> (! #(echo gurka@mndltl01) `#(,(self) hej!))
#(<0.35.0> hej!)
```

Then back in Clojure:

```clojure
(def data (messaging/receive mbox))
(def lfe-pid (tuple-type/get-element data 0))
(messaging/! mbox lfe-pid (types/tuple msg))
```

Then, back in LFE:

```cl
(lfe@mndltl01)> (c:flush)
Shell got {<5926.1.0>,'hello, world'}
```


### Mid-level API [&#x219F;](#table-of-contents)

```clojure
(require '[clojang.core :as clojang]
         '[clojang.mbox :as mbox]
         '[clojang.node :as node]
         '[clojang.types :as types]
         '[clojang.util :as util])

(def gurka (node/new :gurka))
(def inbox (mbox/new gurka :echo))

(def msg [(mbox/get-pid inbox) :hello-world])

(mbox/! inbox :echo :gurka msg)
(mbox/receive inbox)

[#object[com.ericsson.otp.erlang.OtpErlangPid
         0x1fe20514
         "#Pid<gurka@mndltl01.1.0>"]
 :hello-world]
```

From LFE:

```cl
(lfe@mndltl01)> (! #(echo gurka@mndltl01) `#(,(self) hej!))
#(<0.35.0> hej!)
```

Then back in Clojure:

```clojure
(let [[lfe-pid _] (mbox/receive inbox)]
  (mbox/! inbox lfe-pid msg))
```

Then, back in LFE:

```cl
(lfe@mndltl01)> (c:flush)
Shell got {<5926.1.0>,'hello-world'}
```


### Running Tests [&#x219F;](#table-of-contents)

The Clojang tests use metadata annotations to indicate whether they are unit, system, or integration tests. to run just the unit tests, you can do any one of the following, depending upon what you're used to:

```bash
$ lein test
$ lein test :unit
$ lein test :default
```

To run just the system tests:

```bash
$ lein test :system
```

And, similarly, just the integration tests:

```bash
$ lein test :integtration
```

To run everything:

```bash
$ lein test :all
```


## Erlang, Clojure, and JInterface [&#x219F;](#table-of-contents)

If you are interested in building your own JInterface ``.jar`` file for use with a Clojure project, be sure fo check out the page [Building JInterface for Clojure](http://oubiwann.github.io/clojang/current/89-building-jinterface.html) on the Clojang docs site.


## License [&#x219F;](#table-of-contents)

```
Copyright © 2015-2016 Duncan McGreggor

Distributed under the Apache License Version 2.0.
```

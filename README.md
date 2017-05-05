# clojang
[![Build Status][travis-badge]][travis]
[![Dependencies Status][deps-badge]][deps]
[![Clojars Project][clojars-badge]][clojars]

*Erlang/OTP Communications in Clojure (wraps jiface + JInterface)*

[![Clojang logo][logo]][logo-large]


#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Building](#building-)
* [Shells & REPLs](#shells--repls-)
* [Documentation](#documentation-)
* [Usage](#usage-)
* [Running Tests](#running-tests-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

This project provides a final solution to the Clojure
[JInterface Problem](https://github.com/clojang/jiface/wiki/The-JInterface-Problem).
The [jiface low-level API](https://github.com/clojang/jiface) solves this to a
certain extent, but `jiface` requires that programmers perform all their own type
conversions, manual creation of nodes and mboxes, etc.

The Clojang library, however, provides an interface that is not only
syntactically idiomatic Clojure (not unlike the jiface library), but even more
so, provides developers with the same level of convenience they have come to
expect when using Clojure libraries in general, without the need to perform
many manual operations in order to handle Erlang data.

For a comparison of JInterface, the low-level `jiface` API, and the high-level
`clojang` API, see
[the APIs summary](http://clojang.github.io/jiface/current/05-apis.html) page.


## Dependencies [&#x219F;](#contents)

* Java
* Erlang
* lein
* rebar3

The default (and tested) version combinations are as follows:

| Clojang | jiface | JInterface | Erlang Release | Erlang Version (erts) |
|---------|--------|------------|----------------|-----------------------|
| 0.4.0   | 0.4.0  | 1.7.1      | 19.2, 19.3     | 8.2, 8.3              |
| 0.3.0   | 0.3.0  | 1.7.1      | 19.2           | 8.2                   |
| 0.2.0   | 0.2.0  | 1.7.1      | 19.2           | 8.2                   |
| 0.2.0   | 0.2.0  | 1.7.1      | 19.1           | 8.1                   |
| 0.1.0   | 0.1.0  | 1.6.1      | 18.3           | 7.3                   |
| 0.1.0   | 0.1.0  | 1.6.1      | 18.2           | 7.2                   |

While other version combination may work (and existing versions may be updated
to work with different onces), those are the only ones which are supported.


## Building [&#x219F;](#contents)

`rebar3` is used for the top-level builds of the project. It runs `lein` under
the covers in order to build the Clojure code and create the Clojang`.jar`
file. As such, to build everything -- LFE, Erlang, and Clojure -- you need
only do the following:

* `rebar3 compile`

If you wish to build your own JInterface `.jar` file and not use the one we've
uploaded to Clojars, you'll need to follow the instrucations given in the
[jinterface-builder Clojang project](https://github.com/clojang/jinterface-builder).


## Shells & REPLs [&#x219F;](#contents)

There are three interactive programming environments you may start, each of
which will have full access to the project's libraries/dependencies.

LFE:

```bash
$ make lfe-repl
```

Erlang:

```bash
$ rebar3 shell
```

Clojure:

```bash
$ lein clj-repl
```


## Documentation [&#x219F;](#contents)

Project documentation, including Clojang API reference docs, Javadocs for
JInterface, and the Erlang JInterface User's Guide, is availble here:

* [http://clojang.github.io/clojang/current/](http://clojang.github.io/clojang/current/)

Quick links for the other docs:

* Clojang User Guides:
  * [jiface User's Guide](http://clojang.github.io/jiface/current/10-low-level-api.html) -
    A translation of the *JInterface User's Guide* (Erlang documantaion) from
    Java into Clojure; this is refered to as the Clojang low-level API.
  * [Clojang User's Guide](http://clojang.github.io/clojang/current/20-mid-level-api.html) -
    An adaptation of the *jiface User's Guide* which demonstrates the improved
    API that Clojang offers
* [JInterface User's Guide](http://clojang.github.io/jiface/current/erlang/jinterface_users_guide.html) - The JInterface (Java support for Erlang Ports) documentation provided in
  Erlang distributions
* [Jinterface Javadocs](http://clojang.github.io/jiface/current/erlang/java) -
  Javadoc-generated API documentation built from the JInterface source code


## Usage [&#x219F;](#contents)

Using Clojang in a project is just like any other Clojure library. Just add
the following to the `:dependencies` in your `project.clj` file:

[![Clojars Project][clojars-badge]][clojars]

For the Erlang/LFE side of things, you just need to add the Github URL to your
`rebar.config` file, as with any other rebar-based Erlang VM project.

As for actual code usage, the documentation section provides links to
developer guides and API references, but below are also provided some quick
examples.

Here, we'll send a message to our own (default) node's message box, and then
receive it:

```clj
(require '[clojang.core :refer [! receive self]])

(! [(self) :hello-word])
(let [[pid msg] (receive)]
  (println msg))
```

To show remote usage, from an LFE (Lisp Flavoured Erlang) REPL:

```cl
(clojang-lfe@mndltl01)> (! #(default clojang@host) `#(,(self) hej!))
#(<0.35.0> hej!)
```

Then back in Clojure, the same that we used before, but we'll capture the
process id that we sent from LFE and then send another message back to it:

```clj
(let [[pid msg] (receive)]
  (println msg)
  (! pid "hello, world!"))
```

Then, returning to the LFE REPL, we check that we received the message from
Clojang:

```cl
(clojang-lfe@mndltl01)> (c:flush)
Shell got "hello, world!"
ok
```


## Running Tests [&#x219F;](#contents)

All the tests may be run with just one command:

```bash
$ rebar3 eunit
```

This will not only run Erlang and LFE unit tests, it also runs the Clojure
unit tests for Clojang.

**Clojure Test Selectors**

If you would like to be more selective in the types of Clojang tests which get
run, you may be interested in reading this section.

The Clojang tests use metadata annotations to indicate whether they are unit,
system, or integration tests. to run just the unit tests, you can do any one
of the following, depending upon what you're used to:

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

This is what is used by the `rebar3` configuration to run the Clojang tests.


## License [&#x219F;](#contents)

```
Copyright © 2016-2017 Duncan McGreggor

Distributed under the Apache License Version 2.0.
```


<!-- Named page links below: /-->

[travis]: https://travis-ci.org/clojang/clojang
[travis-badge]: https://travis-ci.org/clojang/clojang.png?branch=master
[deps]: http://jarkeeper.com/clojang/clojang
[deps-badge]: http://jarkeeper.com/clojang/clojang/status.svg
[clojars]: https://clojars.org/clojang/clojang
[clojars-badge]: https://img.shields.io/clojars/v/clojang/clojang.svg
[logo]: https://github.com/clojang/resources/blob/master/images/logo-5-250x.png
[logo-large]: https://github.com/clojang/resources/blob/master/images/logo-5-1000x.png

# clojang

[![][clojang-logo]][clojang-logo-large]

[clojang-logo]: resources/images/clojang-logo-250x.png
[clojang-logo-large]: resources/images/clojang-logo-1000x.png

*Erlang's JInterface in Idiomatic Clojure*

![Clojars Project](http://clojars.org/clojang/latest-version.svg)


#### Table of Contents

* [Introduction](#introduction-)
* [Documentation](#documentation-)
* [Usage](#usage-)
  * [Running Tests](#running-tests-)
* [Erlang, Clojure, and JInterface](#erlang-clojure-and-jinterface-)
* [License](#license-)


## Introduction [&#x219F;](#table-of-contents)

TBD


## Documentation [&#x219F;](#table-of-contents)

Project documentation, including Clojang API reference docs, Javadocs for JInterface, and the Erlang JInterface User's Guide, is availble here:

* [http://oubiwann.github.io/clojang/current/](http://oubiwann.github.io/clojang/current/)

Quick links for the other docs:

* [Clojang User's Guide](http://oubiwann.github.io/clojang/current/00-low-level.html) - A translation of the JInterface User's Guide (Erlang) from Java into Clojure
* [JInterface User's Guide](http://oubiwann.github.io/clojang/current/erlang/jinterface_users_guide.html)
* [Jinterface Javadocs](http://oubiwann.github.io/clojang/current/erlang/java)


## Usage [&#x219F;](#table-of-contents)

TBD


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

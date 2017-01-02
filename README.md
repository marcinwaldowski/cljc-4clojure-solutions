# cljc-4clojure-solutions

This project shows how much effort was required to port [4Clojure](http://www.4clojure.com) solutions written in [Clojure](http://clojure.org/) to [ClojureScript](https://github.com/clojure/clojurescript) (see commit log). It also documents some approaches to execute unit test for various ClojureScript host environments.

> **Warning!** Please do not use this project as a source of hints for solving 4clojure problems. It's best to first solve the problem and only after that looking at others solutions.

All unit tests are in file `test/cljc_4clojure_solutions/core_test.cljc`. All non trivial solutions can be found in file `src/cljc_4clojure_solutions/core.cljc`. All other files are just "infrastructure".

## Prerequisites

- Install Java 8.
- [Install](http://leiningen.org/#install) Leiningen.

## Clojure

### Compile

Execute `lein compile :all`. JVM bytecode .class files can be found in `target` directory.

### Run tests

- By Leiningen command.

  Execute `lein test`.

- From REPL.

  Execute `lein repl`. Then from the REPL execute:

  ```clojure
  (require '[clojure.test :as t])
  (require 'cljc-4clojure-solutions.core-test)
  (t/run-tests 'cljc-4clojure-solutions.core-test)
  ```

  In REPL it's easy to test functions directly:

  ```clojure
  (require '[cljc-4clojure-solutions.core :as core])
  (core/num->roman 578)
  (core/prime 40)
  ```

## Clojurescript

### Compile

Compile by executing one of commands below. The result is `testable.js` file under `/resources/private/js` directory.

```sh
lein cljsbuild once none        # no optimization
lein cljsbuild once whitespace  # whitespace optimization
lein cljsbuild once simple      # simple optimization
lein cljsbuild once advanced    # advanced optimization
lein cljsbuild once node        # compilation for Node.js
lein cljsbuild once brepl       # compilation for browser REPL
```
### Running tests with PhantomJS

Make sure [PhantomJS](http://phantomjs.org/) is correctly installed:

```sh
$ phantomjs -v
2.1.1
```

Execute command:

```sh
lein doo phantom whitespace once
```

The `whitespace` can be exchanged with `simple` or `advanced`.

### Running tests with Node.js

Make sure [Node.js](http://nodejs.org) is correctly installed:

```sh
$ node -v
v4.4.5
```

Execute command:

```sh
lein doo node node once
```

### Running tests with Nashorn

Execute command:

```sh
lein doo nashorn whitespace once
```

The `whitespace` can be exchanged with `simple` or `advanced`.

### Running tests with Rhino

Make sure [Rhino](http://www.mozilla.org/rhino) is correctly installed:

```sh
rhino -help
```

Execute command:

```sh
lein doo rhino whitespace once
```

The `whitespace` can be exchanged with `simple` or `advanced`.

### Running tests from browser REPL

Compile sources to JavaScript:

```sh
lein cljsbuild once brepl
```

Open REPL:

```sh
lein repl
```

In REPL start [Weasel](https://github.com/tomjakubowski/weasel) server:

```sh
(start-weasel)
```

It will wait for connection from browser. Open `resources/private/brepl.html ` in modern browser. This causes browser to connect to Weasel and changes REPL prompt to `cljs.user=>`.

Run tests by executing:

```clojure
(require '[cljs.test :as t])
(require 'cljc-4clojure-solutions.core-test)
(t/run-tests 'cljc-4clojure-solutions.core-test)
```

In REPL it's easy to test functions directly:

```clojure
(require '[cljc-4clojure-solutions.core :as core])
(core/num->roman 578)
(core/prime 40)
```

### Running tests from Node.js REPL

Open REPL:

```sh
lein repl
```

In REPL switch to Node.js REPL:

```sh
(start-node)
```

Run tests by executing:

```clojure
(require '[cljs.test :as t])
(require 'cljc-4clojure-solutions.core-test)
(t/run-tests 'cljc-4clojure-solutions.core-test)
```

In REPL it's easy to test functions directly:

```clojure
(require '[cljc-4clojure-solutions.core :as core])
(core/num->roman 578)
(core/prime 40)
```

### Running tests from Nashorn REPL

Proceed the same way as with Node.js REPL but run `(start-nashorn)` instead of `(start-node)`.

### Running tests from Rhino REPL

Proceed the same way as with Node.js REPL but run `(start-rhino)` instead of `(start-node)`.

### Running tests with Planck

Make sure [Planck](https://github.com/mfikes/planck) is correctly installed:

```sh
$ planck -h
```

Run planck with:

```sh
planck -c src:test
```

In planck execute:

```clojure
(require '[clojure.test :as t])
(require 'cljc-4clojure-solutions.core-test)
(t/run-tests 'cljc-4clojure-solutions.core-test)
```

## License

Distributed under the Eclipse Public License version 1.0

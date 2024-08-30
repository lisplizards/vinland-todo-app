# Vinland To Do App

> Demo: create and manage To Do lists

This application is an example of the [Vinland](https://github.com/lisplizards/vinland) web framework.

![App demo](demo.gif)

Features:

* Register an account: claim a unique username and set a password
* Sign in
* Create and delete To Do lists
* Create, delete, and mark complete/incomplete tasks within each To Do list
* Demonstration of setting and removing non-session cookies
* Sign out

Technologies:

* Server: [woo](https://github.com/fukamachi/woo)
* Database: [rucksack](https://gitlab.common-lisp.net/rucksack/rucksack) (Lisp object persistence)
* Framework: [vinland](https://github.com/lisplizards/vinland)
* JavaScript libraries: Hotwire [Turbo](https://github.com/hotwired/turbo) and [Stimulus](https://github.com/hotwired/stimulus), [Shoelace](https://shoelace.style/) web components

Tested on SBCL.

## Installation

It is strongly recommended to use [Qlot](https://github.com/fukamachi/qlot) to manage the project dependencies.

Once you have Qlot installed, clone this repository and install the dependency versions specified in qlfile.lock:

```sh
qlot install
```

### Build

After installing dependencies with Qlot, build the program by running:

```sh
qlot exec sbcl --eval '(asdf:make :vinland-todo-app)'
```

The binary is written to ./bin/todo-app.

The application writes Rucksack data to /tmp/todo-app.

### Run

```sh
./bin/todo-app start -p 8080 -a 0.0.0.0 -w 4
```

## Development

Woo is used as the server when starting the application from the generated binary (see: src/cli.lisp).

You may prefer to start the server from the REPL using clack, which allows for specifying Hunchentoot or another alternative server backend.

Start a REPL, using the Qlot-installed dependencies:

```sh
rlwrap qlot exec sbcl
```

Start the server:

```common-lisp
(ql:quickload '("vinland-todo-app" "clack"))

(clack:clackup todo-app/app:*app* :port 8080 :server :hunchentoot :use-default-middlewares nil :debug nil)
```

### Troubleshooting

To clear the Rucksack database, delete the directory /tmp/todo-app.

## Contributing

Not accepting PRs to this repo, but feel free to open an Issue on GitHub if there's a problem.

## Dependencies

* [cl-bcrypt](https://github.com/dnaeon/cl-bcrypt)
* [clingon](https://github.com/dnaeon/clingon)
* [com.inuoe.jzon](https://github.com/Zulu-Inuoe/jzon)
* [foo.lisp.lack-middleware-charset](https://github.com/lisplizards/lack-middleware-charset)
* [foo.lisp.lack-middleware-debug](https://github.com/lisplizards/lack-middleware-debug)
* [foo.lisp.lack-middleware-errors](https://github.com/lisplizards/lack-middleware-errors)
* [foo.lisp.lack-middleware-head](https://github.com/lisplizards/lack-middleware-head)
* [foo.lisp.lack-middleware-http-methods](https://github.com/lisplizards/lack-middleware-http-methods)
* [foo.lisp.lack-middleware-redact](https://github.com/lisplizards/lack-middleware-redact)
* [foo.lisp.lack-middleware-redis](https://github.com/lisplizards/lack-middleware-redis)
* [foo.lisp.lack-middleware-request-id](https://github.com/lisplizards/lack-middleware-request-id)
* [foo.lisp.lack-middleware-security-headers](https://github.com/lisplizards/lack-middleware-security-headers)
* [foo.lisp.lack-middleware-user](https://github.com/lisplizards/lack-middleware-user)
* [foo.lisp.lack-session-store-redis-pool](https://github.com/lisplizards/lack-session-store-redis-pool)
* [foo.lisp.vinland](https://github.com/lisplizards/vinland)
* [frugal-uuid](https://github.com/ak-coram/cl-frugal-uuid)
* [lack](https://github.com/fukamachi/lack/blob/master/lack.asd)
* [lack-middleware-session](https://github.com/fukamachi/lack/blob/master/lack-middleware-session.asd)
* [lack-middleware-static](https://github.com/fukamachi/lack/blob/master/lack-middleware-static.asd)
* [local-time](https://github.com/dlowe-net/local-time)
* [make-hash](https://github.com/genovese/make-hash)
* [rucksack](https://gitlab.common-lisp.net/rucksack/rucksack)
* [safety-params](https://github.com/fukamachi/safety-params)
* [spinneret](https://github.com/ruricolist/spinneret)
* [trivia](https://github.com/guicho271828/trivia)
* [woo](https://github.com/fukamachi/woo)

### Tests

* [rove](https://github.com/fukamachi/rove)

### JavaScript

* [Turbo](https://github.com/hotwired/turbo)
* [Stimulus](https://github.com/hotwired/stimulus)
* [Shoelace](https://shoelace.style/)

## Author

* John Newton

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0

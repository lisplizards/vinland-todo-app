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

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

### System requirements

* [libev](http://software.schmorp.de/pkg/libev.html) (dependency of Woo)

### Build

Build the program by running:

```lisp
(asdf:make :vinland-todo-app)
```

The binary is written to "./bin/todo-app".

### Run

```sh
./bin/todo-app start -p 8080 -a 0.0.0.0 -w 4
```

## Contributing

Not accepting PRs to this repo, but feel free to open an Issue on GitHub if there's a problem.

## Author

* John Newton

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0

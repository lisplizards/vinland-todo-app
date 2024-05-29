;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "vinland-todo-app"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :build-operation "program-op"
  :build-pathname "bin/todo-app"
  :entry-point "todo-app/cli:main"
  :homepage "https://github.com/lisplizards/vinland-todo-app"
  :bug-tracker "https://github.com/lisplizards/vinland-todo-app/issues"
  :source-control (:git "https://github.com/lisplizards/vinland-todo-app.git")
  :depends-on ("cl-bcrypt"
               "clingon"
               "com.inuoe.jzon"
               "foo.lisp.lack-middleware-charset"
               "foo.lisp.lack-middleware-head"
               "foo.lisp.lack-middleware-http-methods"
               "foo.lisp.lack-middleware-redis"
               "foo.lisp.lack-middleware-request-id"
               "foo.lisp.lack-middleware-security-headers"
               "foo.lisp.lack-middleware-user"
               "foo.lisp.lack-session-store-redis-pool"
               "foo.lisp.vinland"
               "frugal-uuid"
               "lack-middleware-backtrace"
               "lack-middleware-session"
               "lack-middleware-static"
               "lack"
               "local-time"
               "make-hash"
               "rucksack"
               "safety-params"
               "spinneret"
               "trivia"
               "woo")
  :components ((:module "src"
                :components
                ((:file "cli" :depends-on ("app"))
                 (:file "app" :depends-on ("web"))
                 (:file "web" :depends-on ("config" "controller" "params"))
                 (:file "params" :depends-on ("package" "controller"))
                 (:file "controller" :depends-on ("store" "user" "view" "turbo"))
                 (:file "user" :depends-on ("store"))
                 (:file "store" :depends-on ("config" "rucksack"))
                 (:file "view" :depends-on ("component" "layout" "rucksack"))
                 (:file "turbo" :depends-on ("component"))
                 (:file "layout" :depends-on ("component"))
                 (:file "component" :depends-on ("config" "rucksack"))
                 (:file "rucksack" :depends-on ("config"))
                 (:file "config" :depends-on ("package"))
                 (:file "package"))))
  :description "Example application for the Vinland web framework"
  :in-order-to ((test-op (test-op "vinland-todo-app/tests"))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))

(defsystem "vinland-todo-app/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("vinland-todo-app"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for vinland-todo-app"
  :perform (test-op (op c) (symbol-call :rove :run c)))

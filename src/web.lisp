;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/web)

(defparameter *router*
  (foo.lisp.raven:compile-router
   `(("/" ,'root)
     ("/about" ,'about)
     ("/hello-world" ,'hello-world)
     ("/error/server" ,'error/server)
     ("/error/simple" ,'error/simple)
     ("/🧐" ,'🧐)
     ("/login" ,'login)
     ("/register" ,'register)
     ("/action/login" ,'create-login)
     ("/action/register" ,'create-registration)
     ("/action/logout" ,'create-logout)
     ("/lists" ,'todo-lists)
     ("/lists/:todo-list" ,'todo-list)
     ("/lists/:todo-list/items" ,'todo-items)
     ("/lists/:todo-list/items/:todo-item" ,'todo-item))
   :fast-dispatch t)
  "The application router; a lambda function")

(defparameter *web* (funcall *router* :clack)
  "The unwrapped clack application")

;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/cli)

(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))

(defun top-level/command ()
  (clingon:make-command
   :name "todo-app"
   :description "To Do app"
   :long-description (format nil "To Do app: ~
                                  cl-vinland web framework demo")
   :version "0.1.0"
   :license "Apache-2.0"
   :authors '("John Newton")
   :options ()
   :handler #'top-level/handler
   :sub-commands (top-level/sub-commands)))

(defun top-level/handler (cmd)
  "The handler for the top-level command. Will print the usage of the app"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/sub-commands ()
  (list
   (start/command)
   (find-route/command)))

(defun start/command ()
  "Starts the server with the specified options"
  (clingon:make-command
   :name "start"
   :description "Starts the server"
   :options (start/options)
   :handler #'start/handler
   :examples '(("Start the HTTP with default options" . "todo-app start")
               ("Start the HTTP server, listening on port 80" . "todo-app start --port 80")
               ("Start the HTTP server, allowing network access" . "todo-app start --address 0.0.0.0")
               ("Start the HTTP server, specifying the number of workers" . "todo-app start --worker-count 8")
               ("Start the HTTP server with a custom backlog size" . "todo-app start --backlog 256"))))

(defun start/options ()
  (list
   (clingon:make-option :integer
                        :description "Port number"
                        :short-name #\p
                        :long-name "port"
                        :initial-value "8080"
                        :key :port
                        :env-vars '("PORT"))
   (clingon:make-option :string
                        :description "Address"
                        :short-name #\a
                        :long-name "address"
                        :initial-value "127.0.0.1"
                        :key :address
                        :env-vars '("ADDRESS"))
   (clingon:make-option :integer
                        :description "Backlog"
                        :short-name #\c
                        :long-name "backlog"
                        :initial-value 128
                        :key :backlog
                        :env-vars '("BACKLOG"))
   (clingon:make-option :integer
                        :description "Worker count"
                        :short-name #\w
                        :long-name "worker-count"
                        :initial-value 1
                        :key :worker-count
                        :env-vars '("WORKERS"))))

(defun start/handler (cmd)
  (woo:run
   todo-app/app:*app*
   :port (clingon:getopt cmd :port)
   :address (clingon:getopt cmd :address)
   :backlog (clingon:getopt cmd :backlog)
   :worker-num (clingon:getopt cmd :worker-num)
   :debug nil))

(defun find-route/command ()
  (clingon:make-command
   :name "find-route"
   :description "Find the handler for a given path"
   :options ()
   :handler #'find-route/handler
   :examples '(("Find the handler for the base path" . "todo-app find-route /")
               ("Find the handler for a dynamic path" . "todo-app find-route /widgets/abcxyz"))))

(defun find-route/handler (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (assert (= 1 (length args))
            nil
            "find-route takes exactly 1 argument: the path to test")
    (destructuring-bind (path)
        args
      (let ((result (funcall todo-app/web:*router* `(:find-route ,path))))
        (assert result
                nil
                "")
        (format t "~A" result)))))


;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/turbo)

(defun registration/failure (&key username)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) username))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "registration-form-box"
     (:template
      (todo-app/component:registration-form-box
       :username username)))))

(defun login/failure (&key username)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) username))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "login-form-box"
     (:template
      (todo-app/component:login-form-box
       :username username)))))

(defun create-todo-list/success (&key todo-lists)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list todo-lists))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "todo-list-links"
     (:template
      (todo-app/component:todo-list-links
       :todo-lists todo-lists)))
    (:turbo-stream
     :action "replace"
     :target "new-todo-list-form"
     (:template
      (todo-app/component:new-todo-list-form
       :title nil)))))

(defun create-todo-list/failure ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (spinneret:with-html-string
    (:turbo-stream
     :action "update"
     :target "new-todo-list-form-box"
     (:template
      (todo-app/component:new-todo-list-form
       :title nil)))))

(defun delete-todo-list/success (&key todo-list)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-list todo-list))
  (spinneret:with-html-string
    (:turbo-stream
     :action "remove"
     :target (foo.lisp.resource:dom-id todo-list))
    (:turbo-stream
     :action "update"
     :target "flash-container"
     (:template
      (todo-app/component:flash-container)))))

(defun delete-todo-list/failure ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (spinneret:with-html-string
    (:turbo-stream
     :action "update"
     :target "flash-container"
     (:template
      (todo-app/component:flash-container)))))

(defun create-todo-item/success (&key todo-list todo-item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-list todo-list)
           (type todo-app/dao:todo-item todo-item))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "new-todo-item-form"
     (:template
      (todo-app/component:new-todo-item-form
       :todo-list todo-list)))
    (:turbo-stream
     :action "append"
     :target "todo-list-items-list"
     (:template
      (todo-app/component:todo-item
       :todo-item todo-item)))))

(defun create-todo-item/failure (&key todo-list)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-list todo-list))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "new-todo-item-form"
     (:template
      (todo-app/component:new-todo-item-form
       :todo-list todo-list)))
    (:turbo-stream
     :action "replace"
     :target "flash-container"
     (:template
      (todo-app/component:flash-container)))))

(defun update-todo-item/success (&key todo-item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-item todo-item))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "flash-container"
     (:template
      (todo-app/component:flash-container)))
    (:turbo-stream
     :action "replace"
     :target (foo.lisp.resource:dom-id todo-item)
     (:template
      (todo-app/component:todo-item
       :todo-item todo-item)))))

(defun update-todo-item/failure ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (spinneret:with-html-string
      (:turbo-stream
       :action "replace"
       :target "flash-container"
       (:template
        (todo-app/component:flash-container)))))

(defun delete-todo-item/success (&key todo-item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-item todo-item))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "flash-container"
     (:template
      (todo-app/component:flash-container)))
    (:turbo-stream
     :action "remove"
     :target (foo.lisp.resource:dom-id todo-item))))

(defun delete-todo-item/failure ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (spinneret:with-html-string
    (:turbo-stream
     :action "replace"
     :target "flash-container"
     (:template
      (todo-app/component:flash-container)))))

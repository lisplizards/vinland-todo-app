;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/config)

(defvar *system-directory* (asdf:system-source-directory :vinland-todo-app))

(defvar *static-directory* (merge-pathnames #p"static/" *system-directory*))

(defvar *static-errors-directory* (merge-pathnames #p"static/errors/" *system-directory*))

(defvar *rucksack-directory* #p"/tmp/todo-app/")

(defvar *bcrypt-cost* 14)

(defvar *bcrypt-algorithm-identifier* "2a")

(defvar *minimum-password-size* 10)

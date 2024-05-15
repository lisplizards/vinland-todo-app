;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/rucksack)

(rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
  (rucksack:with-transaction ()
    (defclass user ()
      ((user-id :reader user-id
                :initarg :user-id
                :type string
                :index :case-insensitive-string-index
                :unique t
                :documentation "Unique user ID")
       (username :reader username
                 :initarg :username
                 :type string
                 :index :case-insensitive-string-index
                 :unique t
                 :documentation "Unique username used for login")
       (hashed-password :reader hashed-password
                        :initarg :hashed-password
                        :type string
                        :index :case-insensitive-string-index
                        :documentation "The bcrypt password hash")
       (created-at :documentation "Time when the object was created"))
      (:index t)
      (:metaclass rucksack:persistent-class)
      (:documentation "User capable of logging in and creating To Do lists and items"))

    (defmethod initialize-instance :after ((object user) &key)
      (with-slots (user-id created-at) object
        (setf user-id (fuuid:to-string (fuuid:make-v4))
              created-at (get-universal-time))))

    (defmethod print-object ((object user) stream)
      (print-unreadable-object (object stream :type t)
        (with-slots (user-id username) object
	        (format stream "~A: '~A'"
                  username user-id))))

    (defclass todo-list ()
      ((user-id :reader user-id
                :initarg :user-id
                :type string
                :index :case-insensitive-string-index
                :documentation "User ID of the list creator")
       (todo-list-id :reader todo-list-id
                     :initarg :todo-list-id
                     :type string
                     :index :case-insensitive-string-index
                     :unique t
                     :documentation "Unique ID of the To Do list")
       (title :accessor todo-list-title
              :initarg :title
              :type string
              :index :case-insensitive-string-index
              :documentation "Unique title of the To Do list")
       (created-at :documentation "Time when the object was created"))
      (:index t)
      (:metaclass rucksack:persistent-class)
      (:documentation "List of To Do items"))

    (defmethod initialize-instance :after ((object todo-list) &key)
      (with-slots (todo-list-id created-at) object
        (setf todo-list-id (fuuid:to-string (fuuid:make-v4))
              created-at (get-universal-time))))

    (defmethod print-object ((object todo-list) stream)
      (print-unreadable-object (object stream :type t)
        (with-slots (todo-list-id title) object
          (format nil "~A: '~A'" title todo-list-id))))

    (defmethod foo.lisp.resource:%path ((object todo-list))
      (with-slots (todo-list-id) object
        (format nil "/lists/~A" todo-list-id)))

    (defmethod foo.lisp.resource:%dom-id ((object todo-list))
      (with-slots (todo-list-id) object
        (format nil "list-~A" todo-list-id)))

    (defclass todo-item ()
      ((todo-list-id :initarg :todo-list-id
                     :type string
                     :index :case-insensitive-string-index
                     :documentation "Unique ID of the To Do list")
       (todo-item-id :reader todo-item-id
                     :initform (fuuid:to-string (fuuid:make-v4))
                     :type string
                     :index :case-insensitive-string-index
                     :unique t
                     :documentation "Unique ID of the To Do item")
       (content :accessor todo-item-content
                :initarg :content
                :type string
                :documentation "Task text")
       (completedp :accessor todo-item-completed-p
                   :initform nil
                   :type boolean)
       (created-at :documentation "Time when the object was created")
       (updated-at :documentation "Time when the object was last updated"))
      (:index t)
      (:metaclass rucksack:persistent-class)
      (:documentation "An item belonging to a To Do list"))

    (defmethod initialize-instance :after ((object todo-item) &key todo-list)
      (with-slots (created-at updated-at) object
        (let ((now (get-universal-time)))
          (setf created-at now
                updated-at now))))

    (defmethod print-object ((object todo-item) stream)
      (print-unreadable-object (object stream :type t)
        (format stream "To-Do task item: ~A" (slot-value object 'todo-item-id))))

    (defmethod foo.lisp.resource:%path ((object todo-item))
      (with-slots (todo-list-id todo-item-id) object
        (format nil "/lists/~A/items/~A" todo-list-id todo-item-id)))

    (defmethod foo.lisp.resource:%dom-id ((object todo-item))
      (with-slots (todo-item-id) object
        (format nil "item-~A" todo-item-id)))))

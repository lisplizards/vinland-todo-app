;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/dao)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
                       :documentation "Unique ID of the To Do task/item")
         (content :accessor todo-item-content
                  :initarg :content
                  :type string
                  :documentation "Task text")
         (completedp :accessor todo-item-completed-p
                     :initform nil
                     :type boolean
                     :documentation "Whether the task has been completed")
         (created-at :documentation "Time when the object was created")
         (updated-at :documentation "Time when the object was last updated"))
        (:index t)
        (:metaclass rucksack:persistent-class)
        (:documentation "An item belonging to a To Do list")))))

(rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
  (rucksack:with-transaction ()
    (defmethod initialize-instance :after ((object user) &key)
      (with-slots (user-id created-at)
          object
        (setf user-id (fuuid:to-string (fuuid:make-v4))
              created-at (get-universal-time))))

    (defmethod print-object ((object user) stream)
      (print-unreadable-object (object stream :type t)
        (with-slots (user-id username)
            object
          (declare (type (or null string) user-id)
                   (type (or null string) username))
	        (format stream "~A: '~A'"
                  username user-id))))

    (defmethod initialize-instance :after ((object todo-list) &key)
      (with-slots (todo-list-id created-at)
          object
        (setf todo-list-id (fuuid:to-string (fuuid:make-v4))
              created-at (get-universal-time))))

    (defmethod print-object ((object todo-list) stream)
      (print-unreadable-object (object stream :type t)
        (with-slots (todo-list-id title)
            object
          (declare (type (or null string) todo-list-id)
                   (type (or null string) title))
          (format nil "~A: '~A'" title todo-list-id))))

    (defmethod foo.lisp.resource:%path ((object todo-list))
      (with-slots (todo-list-id)
          object
        (declare (type string todo-list-id))
        (route-path 'todo-list :|todo-list| todo-list-id)))

    (defmethod foo.lisp.resource:%dom-id ((object todo-list))
      (with-slots (todo-list-id)
          object
        (declare (type string todo-list-id))
        (format nil "list-~A" todo-list-id)))

    (defmethod initialize-instance :after ((object todo-item) &key)
      (with-slots (created-at updated-at)
          object
        (let ((now (get-universal-time)))
          (declare (type fixnum now))
          (setf created-at now
                updated-at now))))

    (defmethod print-object ((object todo-item) stream)
      (print-unreadable-object (object stream :type t)
        (format stream "To-Do task item: ~A" (the string (slot-value object 'todo-item-id)))))

    (defmethod foo.lisp.resource:%path ((object todo-item))
      (with-slots (todo-list-id todo-item-id)
          object
        (declare (type string todo-list-id todo-item-id))
        (route-path 'todo-item
                    :|todo-list| todo-list-id
                    :|todo-item| todo-item-id)))

    (defmethod foo.lisp.resource:%dom-id ((object todo-item))
      (with-slots (todo-item-id)
          object
        (declare (type string todo-item-id))
        (format nil "item-~A" todo-item-id)))))

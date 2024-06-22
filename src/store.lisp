;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/store)

(defun make-user (&key username password)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string username)
           (type string password))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (unless (>= (the fixnum (length password))
                  (the fixnum todo-app/config:*minimum-password-size*))
        (return-from make-user
          `(:error :insufficient-password-complexity)))
      (let ((bcrypt-password
              (bcrypt:make-password
               password
               :cost todo-app/config:*bcrypt-cost*
               :identifier todo-app/config:*bcrypt-algorithm-identifier*)))
        (handler-case (let ((user (make-instance
                                   'user
                                   :username username
                                   :hashed-password (bcrypt:encode
                                                     bcrypt-password))))
                        `(:ok (:user ,user)))
          (rucksack:btree-key-already-present-error (e)
            (declare (ignore e))
            `(:error :conflict)))))))

(defun list-users ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (let (result)
        (declare (type list result))
        (rucksack:rucksack-map-class
         rs 'user
         #'(lambda (user)
             (declare (type user user))
             (push user result)))
        `(:ok (:users ,result))))))

(defun find-user-by-id (user-id)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string user-id))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (rucksack:rucksack-map-slot
       rs 'user 'user-id
       #'(lambda (user)
           (declare (type user user))
           (return-from find-user-by-id
             `(:ok (:user ,user))))
       :equal user-id)
      '(:error :not-found))))

(defun find-user-by-username (username)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string username))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (rucksack:rucksack-map-slot
       rs 'user 'username
       #'(lambda (user)
           (declare (type user user))
           (return-from find-user-by-username
             `(:ok (:user ,user))))
       :equal username)
      '(:error :not-found))))

(defun make-todo-list (&key user title)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type user user)
           (type string title))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (handler-case `(:ok (:todo-list ,(make-instance 'todo-list
                                                      :user-id (user-id user)
                                                      :title title)))
        (error (e)
          `(:error (:condition ,e)))))))

(defun list-todo-lists (&key user)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type user user))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (handler-case (let ((lists ()))
                      (declare (type list lists))
                      (rucksack:rucksack-map-slot
                       rs 'todo-list 'user-id
                       #'(lambda (todo-list)
                           (declare (type todo-list todo-list))
                           (push todo-list lists))
                       :equal (slot-value user 'user-id))
                      `(:ok (:todo-lists ,(nreverse lists))))
        (error (e)
          `(:error (:condition ,e)))))))

(defun find-todo-list-by-id (todo-list-id)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string todo-list-id))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (rucksack:rucksack-map-slot
       rs 'todo-list 'todo-list-id
       #'(lambda (todo-list)
           (declare (type todo-list todo-list))
           (return-from find-todo-list-by-id
             `(:ok (:todo-list ,todo-list))))
       :equal todo-list-id)
      '(:error :not-found))))

(defun delete-todo-list (&key todo-list)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-list todo-list))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (handler-case (let ((items (list-todo-items :todo-list todo-list)))
                      (declare (type list items))
                      (trivia:ematch
                       items
                       ((list :ok (list :todo-items todo-items))
                        (dolist (todo-item todo-items)
                          (rucksack::rucksack-delete-object rs todo-item))
                        (rucksack::rucksack-delete-object rs todo-list)))
                      :ok)
        (error (e)
          `(:error (:condition ,e)))))))

(defun make-todo-item (&key todo-list content)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-list todo-list)
           (type string content))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (let ((todo-item (make-instance 'todo-item
                                      :todo-list-id (slot-value todo-list 'todo-list-id)
                                      :content content)))
        `(:ok (:todo-item ,todo-item))))))

(defun list-todo-items (&key todo-list)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-list todo-list))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (let ((result ()))
        (rucksack:rucksack-map-slot
         rs 'todo-item 'todo-list-id
         #'(lambda (todo-item)
             (push todo-item result))
         :equal (slot-value todo-list 'todo-list-id))
        `(:ok (:todo-items ,(sort result
                                  #'<
                                  :key #'(lambda (todo-item)
                                           (slot-value todo-item 'created-at)))))))))

(defun find-todo-item-by-id (todo-item-id)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string todo-item-id))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (rucksack:rucksack-map-slot
       rs 'todo-item 'todo-item-id
       #'(lambda (todo-item)
           (declare (type todo-item todo-item))
           (return-from find-todo-item-by-id
             `(:ok (:todo-item ,todo-item))))
       :equal todo-item-id)
      '(:error :not-found))))

(defun delete-todo-item (&key todo-item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-item todo-item))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (handler-case (progn
                      (rucksack::rucksack-delete-object rs todo-item)
                      :ok)
        (error (e)
          `(:error (:condition ,e)))))))

(defun update-todo-item-complete (&key todo-item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-item todo-item))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (setf (todo-item-completed-p todo-item)
            t)
      `(:ok (:todo-item ,todo-item)))))

(defun update-todo-item-incomplete (&key todo-item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-item todo-item))
  (rucksack:with-rucksack (rs todo-app/config:*rucksack-directory*)
    (rucksack:with-transaction ()
      (setf (todo-item-completed-p todo-item)
            nil)
      `(:ok (:todo-item ,todo-item)))))

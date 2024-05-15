;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/params)

(defun string-present-p (val)
  (and (stringp val)
       (> (length val) 0)))

(defun booleanp (val)
  (typep val 'boolean))

(defun validation-error->list (condition)
  `(:error
    (:validation
     (:unpermitted-keys ,(unpermitted-keys condition)
      :missing-keys ,(missing-keys condition)
      :invalid-keys ,(invalid-keys condition)))))

(defmethod %validate-params ((route (eql 'todo-app/controller:create-registration))
                             (request-method (eql :POST))
                             params)
  (declare (ignore route request-method)
           (type list params))
  (handler-case (let ((params
                        (validate
                         (alist
                          (requires "username"
                                    "password"
                                    "password_confirmation")
                          (satisfies
                           "username" #'string-present-p
                           :message "Username is required")
                          (satisfies
                           "password" #'string-present-p
                           :message "Password is required")
                          (satisfies
                           "password_confirmation" #'string-present-p
                           :message "Password confirmation is required"))
                         params)))
                  (let ((password (get-param params "password"))
                        (password-confirmation
                          (get-param params "password_confirmation")))
                    (assert (and password password-confirmation))
                    (unless (string= password password-confirmation)
                      (return-from %validate-params
                        `(:error :password-mismatch)))
                    `(:ok (:username ,(get-param params "username")
                           :password ,password))))
    (validation-error (e)
      (validation-error->list e))))

(defmethod %validate-params ((route (eql 'todo-app/controller:create-login))
                             (request-method (eql :POST))
                             params)
  (declare (ignore route request-method)
           (type list params))
  (handler-case (let ((params (validate
                               (alist
                                (requires "username"
                                          "password")
                                (satisfies
                                 "username" #'string-present-p
                                 :message "Username is required")
                                (satisfies "password" #'string-present-p
                                           :message "Password is required"))
                               params)))
                  (destructuring-bind (username password)
                      (collect-params params '("username"
                                               "password"))
                    `(:ok (:username ,username
                           :password ,password))))
    (validation-error (e)
      (validation-error->list e))))

(defmethod %validate-params ((route (eql 'todo-app/controller:todo-lists))
                             (request-method (eql :POST))
                             params)
  (declare (ignore route request-method)
           (type list params))
  (handler-case (let ((params (validate
                               (alist
                                (requires "title")
                                (satisfies
                                 "title" #'string-present-p
                                 :message "Title is required"))
                               params)))
                  `(:ok (:title ,(get-param params "title"))))
    (validation-error (e)
      (validation-error->list e))))

(defmethod %validate-params ((route (eql 'todo-app/controller:todo-items))
                             (request-method (eql :POST))
                             params)
  (declare (ignore route request-method)
           (type list params))
  (handler-case (let ((params (validate
                               (alist
                                (requires "content")
                                (satisfies "content" #'string-present-p
                                           :message "Task may not be blank"))
                               params)))
                  `(:ok (:content ,(get-param params "content"))))
    (validation-error (e)
      (validation-error->list e))))

(defmethod %validate-params ((route (eql 'todo-app/controller:todo-item))
                             (request-method (eql :PATCH))
                             params)
  (declare (ignore route request-method)
           (type list params))
  (handler-case (let ((params (validate
                               (alist
                                (requires "completed")
                                (satisfies "completed" #'booleanp
                                           :message "'completed' must be a boolean"))
                               params)))
                  `(:ok (:completed ,(get-param params "completed"))))
    (validation-error (e)
      (validation-error->list e))))

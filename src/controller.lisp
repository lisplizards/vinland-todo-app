;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/controller)

(defun cache-control ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (set-response-headers :cache-control "private, no-store, must-revalidate"))

(define-controller 'root
  :export t
  :method '(:HEAD :GET :OPTIONS)
  :before  (list #'cache-control #'require-no-login)
  :GET (lambda ()
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (redirect "/login"))
  :documentation "The root resource redirects to the current user's lists collection resource when
there is a current session, otherwise redirects to the sign-in page")

(define-controller 'about
  :export t
  :method '(:HEAD :GET :OPTIONS)
  :provide "text/html"
  :before (list #'cache-control)
  :GET (lambda ()
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (with-redis (:page-visits)
           (render :headers '(:content-type "text/html")
                   :view #'todo-app/view:about
                   :args `(:page-hits ,(or (red:get "hits") 0)))))
  :documentation "About the To Do demo application")

(export 'hello-world)
(defun hello-world (env)
  "Demonstration of a \"bare\" handler"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (ignore env))
  `(200
    (:content-type "text/plain"
     :content-length 13)
    ("Hello, World.")))

(define-controller '🧐
  :export t
  :method '(:HEAD :GET :OPTIONS)
  :provide "text/html"
  :before (list #'cache-control)
  :GET (lambda ()
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (render :headers '(:content-type "text/html")
                 :view #'todo-app/view:🧐))
  :documentation "Emoji page...")

(define-controller 'register
  :export t
  :method '(:HEAD :GET :OPTIONS)
  :provide "text/html"
  :before (list #'cache-control #'require-no-login)
  :GET (lambda ()
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         `(200 (:content-type "text/html")
               (,(todo-app/view:register))))
  :documentation "User account registration page")

(define-controller 'create-registration
  :export t
  :method '(:POST :OPTIONS)
  :accept "application/x-www-form-urlencoded"
  :before (list #'cache-control #'require-no-login)
  :POST (lambda ()
          (declare (optimize (speed 3) (safety 0) (debug 0)))
          (block nil
            (flet ((fail (&key message username)
                     (declare (type (or html-safe string) message)
                              (type (or null string) username))
                     (flash-now :error message)
                     (return
                       (render :status :unprocessable-content
                               :headers '(:content-type "text/vnd.turbo-stream.html")
                               :view #'todo-app/turbo:registration-failure
                               :args `(:username ,username)))))
              (trivia:ematch
               (validate-params (body-params))
               ((list :ok (trivia:plist :username username
                                        :password password))
                (declare (type string username password))
                (trivia:ematch
                 (store:make-user :username username
                                  :password password)
                 ((list :ok (list :user _))
                  (redirect "/login" :status :see-other
                                     :flash '(:success "Account created. Please login to continue.")))
                 ((list :error :insufficient-password-complexity)
                  (fail :message "Password is not sufficiently complex."
                        :username username))
                 ((list :error :conflict)
                  (fail :message "Username already exists. Please choose another."
                        :username username))))
               ((list :error :password-mismatch)
                (fail :message "Passwords do not match."
                      :username (get-body-param "username")))
               ((list :error (list :validation (trivia:plist
                                                :unpermitted-keys unpermitted-keys
                                                :missing-keys missing-keys
                                                :invalid-keys invalid-keys)))
                (let ((username (get-body-param "username")))
                  (when unpermitted-keys
                    (fail :username username
                          :message "Unpermitted parameters detected. Stop."))
                  (when missing-keys
                    (fail :username username
                          :message (format nil "Request is missing the following parameters: ~{~A~^ ~}"
                                           missing-keys)))
                  (when invalid-keys
                    (let ((error-messages (remove nil (collect-params
                                                       invalid-keys
                                                       '("username"
                                                         "password"
                                                         "password_confirmation")))))
                      (fail :username username
                            :message (html-safe (spinneret:with-html-string
                                                    (mapcar
                                                     (lambda (message)
                                                       (:p message))
                                                     error-messages))))))))))))
  :documentation "Registers a new user account")

(define-controller 'login
  :export t
  :method '(:HEAD :GET :OPTIONS)
  :provide "text/html"
  :before (list #'cache-control #'require-no-login)
  :GET (lambda ()
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (set-response-headers :content-type "text/html")
         (todo-app/view:login))
  :documentation "Sign-in page")

(define-controller 'create-login
  :export t
  :method '(:POST :OPTIONS)
  :accept "application/x-www-form-urlencoded"
  :before (list #'cache-control #'require-no-login)
  :POST (lambda ()
          (declare (optimize (speed 3) (safety 0) (debug 0)))
          (block nil
            (flet ((fail (&key message username)
                     (declare (type (or html-safe string) message)
                              (type (or null string) username))
                     (flash-now :error message)
                     (return
                       (render :status :unprocessable-content
                               :headers '(:content-type "text/vnd.turbo-stream.html")
                               :view #'todo-app/turbo:login-failure
                               :args `(:username ,username)))))
              (let ((invalid-credential-message "Username/password is invalid"))
                (trivia:ematch
                 (validate-params (body-params))
                 ((list :ok (trivia:plist :username username
                                          :password password))
                  (trivia:ematch
                   (todo-app/store:find-user-by-username username)
                   ((list :ok (list :user user))
                    (unless (bcrypt:password= password
                                              (todo-app/rucksack:hashed-password
                                               user))
                      (fail :message invalid-credential-message))
                    (set-session-options '(:change-id t :new-session t :expire nil))
                    (set-session :user (hash (:id (todo-app/rucksack:user-id user))
                                             (:username username)
                                             (:time (get-universal-time))))
                    (let ((example-cookie-config (list
                                                  :value (com.inuoe.jzon:stringify
                                                          (hash
                                                           ("foo" "bar")
                                                           ("baaz" (hash
                                                                    ("quux" "foobar")))
                                                           ("quux" '(1 2 3)))
                                                          :stream nil
                                                          :pretty nil)
                                                  :path "/"
                                                  :domain "localhost"
                                                  :expires (+ 1200 (get-universal-time))
                                                  :httponly t
                                                  :secure nil
                                                  :samesite :strict)))
                      (set-cookies `(("_example" . ,example-cookie-config)
                                     ("_foo" . (:value "ok"
                                                :path "/"
                                                :httponly t
                                                :secure nil
                                                :samesite :strict)))))
                    (redirect (if (session :origin)
                                  (session :origin)
                                  "/lists")
                              :status 303
                              :flash '(:success "Sign-in successful")))
                   ((list :error :not-found)
                    ;; Simulate time it takes to run bcrypt iterations.
                    (sleep 0.2)
                    (fail :message invalid-credential-message))))
                 ((list :error (list :validation
                                     (trivia:plist
                                      :unpermitted-keys unpermitted-keys
                                      :missing-keys missing-keys
                                      :invalid-keys invalid-keys)))
                  (when unpermitted-keys
                    (fail :message "Unpermitted parameters detected. Stop."))
                  (when missing-keys
                    (fail :message (format nil "Request is missing the following parameters: ~{~A ~}"
                                           missing-keys)))
                  (when invalid-keys
                    (let ((error-messages (remove nil (collect-params invalid-keys
                                                                      '("username"
                                                                        "password")))))
                      (fail :message (html-safe
                                      (format nil "~{<p>~A</p>~}" error-messages)))))))))))
  :documentation "Given a valid username-password combination, creates a user session and
redirects to /lists")

(define-controller 'create-logout
  :export t
  :method '(:POST :OPTIONS)
  :accept "application/x-www-form-urlencoded"
  :before (list #'cache-control #'require-login)
  :POST (lambda ()
          (declare (optimize (speed 3) (safety 0) (debug 0)))
          (set-session-options '(:change-id t :new-session t :expire nil))
          (clear-session)
          (delete-cookie "_example")
          (delete-cookie "_foo")
          (redirect "/login" :status 303
                             :flash '(:success "Signed out")))
  :documentation "Signs out the current user, clearing the session and cookies")

(define-controller 'todo-lists
  :export t
  :method '(:HEAD :GET :POST :OPTIONS)
  :accept "application/x-www-form-urlencoded"
  :provide "text/html"
  :before (list #'cache-control #'require-login)
  :GET (lambda ()
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (trivia:ematch
          (todo-app/store:list-todo-lists :user *current-user*)
          ((list :ok (list :todo-lists todo-lists))
           (render :headers '(:content-type "text/html")
                   :view #'todo-app/view:todo-lists
                   :args `(:todo-lists ,todo-lists)))
          ((list :error (list :condition _))
           (server-error :service-unavailable))))
  :POST (lambda ()
          (declare (optimize (speed 3) (safety 0) (debug 0)))
          (block nil
            (flet ((fail (&key message)
                     (declare (type (or html-safe string) message))
                     (flash-now :error message)
                     (return
                       (render :status :unprocessable-content
                               :headers '(:content-type "text/vnd.turbo-stream.html")
                               :view #'todo-app/turbo:create-todo-list-failure))))
              (trivia:ematch
               (validate-params (body-params))
               ((list :ok (list :title title))
                (trivia:ematch
                 (todo-app/store:make-todo-list :user *current-user*
                                                :title title)
                 ((list :ok (trivia:plist :todo-list _))
                  (trivia:ematch
                   (todo-app/store:list-todo-lists :user *current-user*)
                   ((list :ok (list :todo-lists todo-lists))
                    (negotiate
                     ("text/vnd.turbo-stream.html"
                      (render :headers '(:content-type "text/vnd.turbo-stream.html")
                              :view #'todo-app/turbo:create-todo-list-success
                              :args `(:todo-lists ,todo-lists)))))
                   ((list :error (list :condition _))
                    (fail :message "Failed to query lists"))))
                 ((list :error (list :condition _))
                  (fail :message "Failed to create list"))))
               ((list :error (list :validation (trivia:plist
                                                :unpermitted-keys unpermitted-keys
                                                :missing-keys missing-keys
                                                :invalid-keys invalid-keys)))
                (when unpermitted-keys
                  (fail :message "Unpermitted parameters detected. Stop."))
                (when missing-keys
                  (fail :message (format nil "Request is missing the following parameters: ~{~A ~}"
                                         missing-keys)))
                (when invalid-keys
                  (fail :message (get-param invalid-keys "title"))))))))
  :documentation "To Do lists collection resource")

(defun find-todo-list ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (trivia:ematch
   (todo-app/store:find-todo-list-by-id (binding :|todo-list|))
   ((list :ok (list :todo-list todo-list))
    (declare (type todo-app/rucksack:todo-list todo-list))
    (unless (equal (todo-app/rucksack:user-id *current-user*)
                   (todo-app/rucksack:user-id todo-list))
      (client-error :forbidden))
    (trivia:ematch
     (todo-app/store:list-todo-items :todo-list todo-list)
     ((list :ok (list :todo-items todo-items))
      (declare (type list todo-items))
      `(:todo-list ,todo-list
        :todo-items ,todo-items))
     ((list :error (list :condition _))
      (server-error :internal-server-error))))
   ((list :error :not-found)
    (client-error :not-found))))

(define-controller 'todo-list
  :export t
  :method '(:HEAD :GET :DELETE :OPTIONS)
  :provide "text/html"
  :before (list #'cache-control #'require-login)
  :GET (lambda ()
         (declare (optimize (speed 3) (safety 0) (debug 0)))
         (trivia:ematch
          (find-todo-list)
          ((trivia:plist :todo-list todo-list :todo-items todo-items)
           (declare (type todo-app/rucksack:todo-list todo-list)
                    (type list todo-items))
           (render :headers '(:content-type "text/html")
                   :view #'todo-app/view:todo-list
                   :args `(:todo-list ,todo-list :todo-items ,todo-items)))))
  :DELETE (lambda ()
            (declare (optimize (speed 3) (safety 0) (debug 0)))
            (trivia:ematch
             (find-todo-list)
             ((trivia:plist :todo-list todo-list :todo-items _)
              (trivia:ematch
               (todo-app/store:delete-todo-list :todo-list todo-list)
               (:ok
                (declare (type todo-app/rucksack:todo-list todo-list))
                (negotiate
                 ("text/vnd.turbo-stream.html"
                  (flash-now :success "List deleted")
                  (render :headers '(:content-type "text/vnd.turbo-stream.html")
                          :view #'todo-app/turbo:delete-todo-list-success
                          :args `(:todo-list ,todo-list)))))
               ((list :error (list :condition _))
                (flash-now :error "Failed to delete list")
                (render :status :unprocessable-content
                        :headers '(:content-type "text/vnd.turbo-stream.html")
                        :view #'todo-app/turbo:delete-todo-list-failure))))))
  :documentation "To Do List resource")

(define-controller 'todo-items
  :export t
  :method '(:POST :OPTIONS)
  :accept "application/x-www-form-urlencoded"
  :before (list #'cache-control #'require-login)
  :POST (lambda ()
          (declare (optimize (speed 3) (safety 0) (debug 0)))
          (block nil
            (flet ((fail (&key message)
                     (declare (type string message))
                     (flash-now :error message)
                     (return
                       (render :status 422
                               :headers '(:content-type "text/vnd.turbo-stream.html")
                               :view #'todo-app/turbo:create-todo-item-failure))))
              (trivia:ematch
               (validate-params (body-params))
               ((list :ok (list :content content))
                (trivia:ematch
                 (todo-app/store:find-todo-list-by-id (binding :|todo-list|))
                 ((list :ok (list :todo-list todo-list))
                  (unless (equal (slot-value *current-user* 'todo-app/rucksack:user-id)
                                 (slot-value todo-list 'todo-app/rucksack:user-id))
                    (client-error :not-found))
                  (trivia:ematch
                   (todo-app/store:make-todo-item :todo-list todo-list
                                                  :content content)
                   ((list :ok (list :todo-item todo-item))
                    (render :headers '(:content-type "text/vnd.turbo-stream.html")
                            :view #'todo-app/turbo:create-todo-item-success
                            :args `(:todo-list ,todo-list
                                    :todo-item ,todo-item)))))
                 ((list :error :not-found)
                  (client-error :not-found))))
               ((list :error (list :validation (trivia:plist
                                                :unpermitted-keys unpermitted-keys
                                                :missing-keys missing-keys
                                                :invalid-keys invalid-keys)))
                (when unpermitted-keys
                  (fail :message "Unpermitted parameters detected. Stop."))
                (when missing-keys
                  (fail :message (format nil "Request is missing the following parameters: ~{~A ~}"
                                         missing-keys)))
                (when invalid-keys
                  (fail :message (get-param invalid-keys "content"))))))))
  :documentation "To-Do items collection resource for a specific list")

(defun find-todo-item ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (trivia:ematch
   (todo-app/store:find-todo-list-by-id (binding :|todo-list|))
   ((list :ok (list :todo-list todo-list))
    (unless (equal (slot-value *current-user* 'todo-app/rucksack:user-id)
                   (slot-value todo-list 'todo-app/rucksack:user-id))
      (client-error :forbidden))
    (trivia:ematch
     (todo-app/store:find-todo-item-by-id (binding :|todo-item|))
     ((list :ok (list :todo-item todo-item))
      (unless (equal (slot-value todo-item 'todo-app/rucksack:todo-list-id)
                     (slot-value todo-list 'todo-app/rucksack:todo-list-id))
        (client-error :not-found))
      (list :todo-list todo-list
            :todo-item todo-item))
     ((list :error :not-found)
      (client-error :not-found))))
   ((list :error :not-found)
    (client-error :not-found))))

(define-controller 'todo-item
  :export t
  :method '(:PATCH :DELETE :OPTIONS)
  :accept "application/json"
  :before (list #'cache-control #'require-login)
  :PATCH (lambda ()
           (declare (optimize (speed 3) (safety 0) (debug 0)))
           (block nil
             (flet ((fail (&key message)
                      (declare (type (or html-safe string) message))
                      (flash-now :error message)
                      (return
                        (render :status :unprocessable-content
                                :headers '(:content-type "text/vnd.turbo-stream.html")
                                :view #'todo-app/turbo:update-todo-item-failure))))
               (trivia:ematch
                (find-todo-item)
                ((trivia:plist :todo-list _ :todo-item todo-item)
                 (trivia:ematch
                  (validate-params (body-params))
                  ((list :ok (list :completed completedp))
                   ;; Idempotent update: ignore update return value and
                   ;; return 200 whether or not already complete/incomplete
                   (if completedp
                       (todo-app/store:update-todo-item-complete :todo-item todo-item)
                       (todo-app/store:update-todo-item-incomplete :todo-item todo-item))
                   (flash-now :success "Task updated")
                   (render :status :ok
                           :headers '(:content-type "text/vnd.turbo-stream.html")
                           :view #'todo-app/turbo:update-todo-item-success
                           :args `(:todo-item ,todo-item)))
                  ((list :error (list :validation (trivia:plist
                                                   :unpermitted-keys unpermitted-keys
                                                   :missing-keys missing-keys
                                                   :invalid-keys invalid-keys)))
                   (when unpermitted-keys
                     (fail :message "Unpermitted parameters detected. Stop."))
                   (when missing-keys
                     (fail :message (format nil "Request is missing the following parameters: ~{~A ~}"
                                            missing-keys)))
                   (when invalid-keys
                     (let ((error-messages (remove nil (collect-params invalid-keys
                                                                       '("status"
                                                                         "content")))))
                       (fail :message (html-safe
                                       (format nil "~{<p>~A</p>~}" error-messages))))))))))))
  :DELETE (lambda ()
            (declare (optimize (speed 3) (safety 0) (debug 0)))
            (trivia:ematch
             (find-todo-item)
             ((trivia:plist :todo-list _ :todo-item todo-item)
              (trivia:ematch
               (todo-app/store:delete-todo-item :todo-item todo-item)
               (:ok
                (flash-now :success "Task deleted")
                (render :headers '(:content-type "text/vnd.turbo-stream.html")
                        :view #'todo-app/turbo:delete-todo-item-success
                        :args `(:todo-item ,todo-item)))
               ((list :error (list :condition _))
                (flash-now :error "Failed to delete task")
                (render :status :unprocessable-content
                        :headers '(:content-type "text/vnd.turbo-stream.html")
                        :view #'todo-app/turbo:delete-todo-item-failure))))))
  :documentation "To Do item resource; a single task in a To Do list")

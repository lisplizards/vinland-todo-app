;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/component)

(defun csrf-input ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (spinneret:with-html
      (:input :type "hidden"
              :class "hidden"
              :autocomplete "off"
              :name lack/middleware/csrf::*csrf-middleware-token*
              :value (foo.lisp.vinland/web:csrf-token))))

(defun flash-container (&optional (flash-types '(:notice :success :alert :error)))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type foo.lisp.vinland/handler/types:keyword-list flash-types))
  (unless foo.lisp.vinland:*flash*
    (return-from flash-container
      (spinneret:with-html
          (:div :class "flash-container"))))
  (spinneret:with-html
      (:div :id "flash-container"
            :class "flash-container"
            :data-turbo-temporary t
            (mapcar
             #'(lambda (flash-type)
                 (declare (type keyword flash-type))
                 (let ((message (flash:get-flash flash-type)))
                   (when message
                     (flash :type flash-type
                            :message message))))
             flash-types))))

(defun flash (&key type message)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type keyword type)
           (type (or null string foo.lisp.vinland/web:html-safe) message))
  (assert (not (null message))
          nil
          "Flash message is empty")
  (spinneret:with-html
      (flet ((message ()
               (etypecase message
                 (string message)
                 (foo.lisp.vinland/web:html-safe
                  (:raw
                   (foo.lisp.vinland/web:html-safe-value message))))))
        (ecase type
          (:notice
           (:sl-alert :variant "primary" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "info-circle")
                      (message)))
          (:success
           (:sl-alert :variant "success" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "check2-circle")
                      (message)))
          (:alert
           (:sl-alert :variant "warning" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "exclamation-triangle")
                      (message)))
          (:error
           (:sl-alert :variant "danger" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "exclamation-octagon")
                      (message)))))))

(defun site-header ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (spinneret:with-html
      (:header
       (:div :class "site-header-left"
             (:p :class "site-title"
                 (:a :href "/" "ToDo app"))
             (:a :class "social-link"
                 :href "https://github.com/lisplizards/vinland-todo-app"
                 :target "_blank"
                 :rel "noopener noreferrer"
                 (:img :src "/images/github-mark.svg")))
       (if *current-user*
           (:nav
            (:sl-button :href "/lists"
                        "Lists")
            (:form :action "/action/logout"
                   :method "POST"
                   :data-turbo-frame "_top"
                   (csrf-input)
                   (:sl-button :type "submit"
                               :id "sign-out-button"
                               "Sign out")))
           (:nav
            (:sl-button :href "/login"
                        "Sign in")
            (:sl-button :href "/register"
                        "Register"))))))

(defun site-footer ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (spinneret:with-html
      (:footer
       (:div :class "copyright-container"
             (:p :class "footer-copyright" "© John Newton 2024"))
       (:nav
        (:a :href "/about"
            "About")))))

(defun session-spinner (&key message hidden)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) message)
           (type boolean hidden))
  (spinneret:with-html
      (:div
       :class (format nil "session-spinner ~A" (format nil "~A"
                                                       (if hidden "hidden" "")))
       :data-session-spinner-target "spinner"
       (:sl-alert :variant "warning"
                  :open t
                  :class "temp-alert"
                  (:sl-icon :slot "icon" :name "exclamation-triangle")
                  message)
       (:div :class "spinner-box"
             (:sl-spinner)))))

(defun registration-form-box (&key username)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) username))
  (spinneret:with-html
      (:div :id "registration-form-box"
            :class "registration-form-box"
            :data-controller "session-spinner"
            :data-session-spinner-hidden-class "hidden"
            (session-spinner
             :message "Please stand by while we create your account. Do not refresh the page."
             :hidden t)
            (registration-form :username username))))

(defun registration-form (&key username)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) username))
  (spinneret:with-html
      (:form :action "/action/register"
             :accept-charset "UTF-8"
             :method "POST"
             :class "registration-form"
             :data-session-spinner-target "form"
             :data-action "submit->session-spinner#showSpinner"
             :data-turbo "true"
             :data-turbo-frame "_top" ;; replace page body
             (flash-container)
             (csrf-input)
             (:sl-input :label "Username"
                        :type "text"
                        :name "username"
                        :autofocus t
                        :value username)
             (:br)
             (:sl-input :label "Password"
                        :type "password"
                        :name "password"
                        :password-toggle t)
             (:br)
             (:sl-input :label "Confirm password"
                        :type "password"
                        :name "password_confirmation"
                        :password-toggle t)
             (:br)
             (:div :class "form-actions"
                   (:sl-button :type "submit"
                               :variant "primary"
                               "Submit")))))

(defun login-form-box (&key username)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) username))
  (spinneret:with-html
      (:div :class "login-form-box"
            :id "login-form-box"
            :data-controller "session-spinner"
            :data-session-spinner-hidden-class "hidden"
            (session-spinner
             :message "Checking account credentials"
             :hidden t)
            (todo-app/component:login-form :username username))))

(defun login-form (&key username)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) username))
  (spinneret:with-html
      (:form :action "/action/login"
             :accept-charset "UTF-8"
             :method "POST"
             :class "login-form"
             :data-session-spinner-target "form"
             :data-action "submit->session-spinner#showSpinner"
             :data-turbo "true"
             :data-turbo-frame "_top" ;; replace page body
             (flash-container)
             (csrf-input)
             (:sl-input :label "Username"
                        :type "text"
                        :name "username"
                        :autofocus t
                        :value username)
             (:br)
             (:sl-input :label "Password"
                        :type "password"
                        :name "password")
             (:br)
             (:sl-button :type "submit"
                         :variant "primary"
                         "Sign in"))))

(defun new-todo-list-form (&key title)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null string) title))
  (spinneret:with-html
      (:form :action "/lists"
             :accept-charset "UTF-8"
             :method "POST"
             :class "new-todo-list-form"
             :id "new-todo-list-form"
             (flash-container)
             (csrf-input)
             (:div :class "new-todo-list-inputs-box"
                   (:sl-input :placeholder "Title"
                              :name "title"
                              :autofocus t
                              :value title)
                   (:sl-button :type "submit"
                               :variant "primary"
                               "Create list")))))

(defun todo-list-links (&key todo-lists)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list todo-lists))
  (spinneret:with-html
      (unless todo-lists
        (return-from todo-list-links
          (:div :class "todo-list-links" :id "todo-list-links"
                (:p "No lists have been created yet."))))
    (:div :class "todo-list-links" :id "todo-list-links"
          (:ul :id "todo-list-links-list" :class "todo-list-links-list"
               (mapcar
                #'(lambda (todo-list)
                    (todo-list-link-item :todo-list todo-list))
                todo-lists)))))

(defun todo-list-link-item (&key todo-list)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-list todo-list))
  (spinneret:with-html
      (:li :id (foo.lisp.resource:dom-id todo-list)
           (:div :class "todo-list-link-item__link-box"
                 (:p :class "todo-list-link-item__title"
                     (todo-app/dao:todo-list-title todo-list))
                 (:p (:small (format nil "Created: ~A"
                                     (local-time:format-timestring
                                      nil
                                      (local-time:universal-to-timestamp
                                       (slot-value todo-list 'todo-app/dao:created-at))
                                      :format local-time:+rfc-1123-format+)))))
           (:div :class "todo-list-link-item__controls"
                 (:sl-button :variant "primary"
                             :href (foo.lisp.resource:path todo-list)
                             "View")
                 (:form :action (foo.lisp.resource:path todo-list)
                        :method "DELETE"
                        (csrf-input)
                        (:sl-button :variant "danger"
                                    :type "submit"
                                    "Delete"))))))

(defun new-todo-item-form (&key todo-list)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-list todo-list))
  (spinneret:with-html
      (:form :accept-charset "UTF-8"
             :method "POST"
             :id "new-todo-item-form"
             :class "new-todo-item-form"
             :action (format nil "~A/items" (foo.lisp.resource:path todo-list))
             (csrf-input)
             (:sl-input :name "content"
                        :autofocus t
                        :placeholder "Task description"))))

(defun todo-item (&key todo-item)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/dao:todo-item todo-item))
  (spinneret:with-html
      (with-slots (todo-app/dao:todo-item-id todo-app/dao:content)
          todo-item
        (:li :class "todo-list-item"
             :id (foo.lisp.resource:dom-id todo-item)
             (:form :method "PATCH"
                    :action (foo.lisp.resource:path todo-item)
                    :class "todo-list-item-patch-form"
                    (csrf-input)
                    (if (todo-app/dao:todo-item-completed-p todo-item)
                        (:sl-checkbox :name "completed"
                                      :checked t
                                      (:p (:s todo-app/dao:content)))
                        (:sl-checkbox :checked nil
                                      :name "completed"
                                      (:p todo-app/dao:content))))
             (:form :method "DELETE"
                    :action (foo.lisp.resource:path todo-item)
                    (csrf-input)
                    (:sl-button :variant "default"
                                :type "submit"
                                (:sl-icon :name "x-lg")))))))

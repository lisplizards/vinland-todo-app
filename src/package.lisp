;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defpackage #:todo-app/config
  (:use #:cl)
  (:export #:*system-directory*
           #:*static-directory*
           #:*static-errors-directory*
           #:*rucksack-directory*
           #:*bcrypt-cost*
           #:*minimum-password-size*
           #:*bcrypt-algorithm-identifier*)
  (:documentation "Package containing configuration-related special variables."))

(defpackage #:todo-app/dao
  (:use #:cl)
  (:import-from #:foo.lisp.vinland/web
                #:route-path)
  (:export #:user
           #:todo-list
           #:todo-item)
  (:export #:user-id
           #:username
           #:hashed-password
           #:todo-list-id
           #:title
           #:todo-item-id
           #:content
           #:completedp
           #:created-at
           #:updated-at)
  (:export #:todo-list-title
           #:todo-item-content
           #:todo-item-completed-p)
  (:documentation "Rucksack persistent class definitions"))

(defpackage #:todo-app/store
  (:use #:cl #:todo-app/dao)
  (:export #:make-user
           #:list-users
           #:find-user-by-id
           #:find-user-by-username
           #:make-todo-list
           #:list-todo-lists
           #:find-todo-list-by-id
           #:delete-todo-list
           #:make-todo-item
           #:list-todo-items
           #:find-todo-item-by-id
           #:delete-todo-item
           #:update-todo-item-complete
           #:update-todo-item-incomplete)
  (:documentation "Package containing Rucksack persistence related functions."))

(defpackage #:todo-app/component
  (:local-nicknames (#:vinland #:foo.lisp.vinland)
                    (#:flash #:foo.lisp.flash))
  (:use #:cl)
  (:import-from #:foo.lisp.vinland/web
                #:route-path)
  (:import-from #:lack/middleware/user
                #:*current-user*)
  (:export #:site-header
           #:site-footer
           #:flash-container
           #:flash
           #:csrf-input
           #:session-spinner
           #:registration-form-box
           #:registration-form
           #:login-form-box
           #:login-form
           #:new-todo-list-form
           #:todo-list-links
           #:todo-list-link-item
           #:todo-list
           #:new-todo-item-form
           #:todo-item)
  (:documentation "Package containing building-block functions to compose HTML views."))

(defpackage #:todo-app/layout
  (:use #:cl)
  (:export #:with-main-layout)
  (:documentation "Package containing HTML layout macros called from view functions."))

(defpackage #:todo-app/http-error
  (:use #:cl)
  (:import-from #:todo-app/config
                #:*static-errors-directory*)
  (:import-from #:todo-app/layout
                #:with-main-layout)
  (:export #:*static-file-types*
           #:*required-handlers*
           #:*http-errors*)
  (:export #:generate-static)
  (:documentation "Package containing functions used to render HTTP error responses."))

(defpackage #:todo-app/view
  (:local-nicknames (#:flash #:foo.lisp.flash))
  (:use #:cl)
  (:import-from #:foo.lisp.vinland/web
                #:route-path)
  (:import-from #:todo-app/layout
                #:with-main-layout)
  (:export #:about
           #:🧐
           #:login
           #:register
           #:todo-lists
           #:todo-list)
  (:documentation "Package containing functions to render complete HTML documents (Content-Type: text/html)."))

(defpackage #:todo-app/turbo
  (:use #:cl)
  (:export #:registration/failure
           #:registration/success
           #:login/failure
           #:login/success
           #:create-todo-list/success
           #:create-todo-list/failure
           #:delete-todo-list/success
           #:delete-todo-list/failure
           #:create-todo-item/success
           #:create-todo-item/failure
           #:update-todo-item/success
           #:update-todo-item/failure
           #:delete-todo-item/success
           #:delete-todo-item/failure)
  (:documentation "Package containing functions to render Hotwire Turbo documents (Content-Type: text/vnd.turbo-stream.html)."))

(defpackage #:todo-app/user
  (:local-nicknames (#:vinland #:foo.lisp.vinland))
  (:use #:cl)
  (:import-from #:lack/middleware/user
                #:*current-user*)
  (:import-from #:foo.lisp.vinland/web
                #:halt
                #:redirect)
  (:export #:current-user
           #:require-login
           #:require-no-login
           #:redirect-to-sign-in)
  (:documentation "Package containing functions related to user sessions"))

(defpackage #:todo-app/controller
  (:local-nicknames (#:vinland #:foo.lisp.vinland)
                    (#:flash #:foo.lisp.flash)
                    (#:store #:todo-app/store))
  (:use #:cl)
  (:import-from #:make-hash
                #:make-hash)
  (:import-from #:lack/middleware/user
                #:*current-user*)
  (:import-from #:lack/middleware/redis
                #:with-redis)
  (:import-from #:foo.lisp.flash
                #:clear-flash
                #:get-flash
                #:flash
                #:flash-now
                #:flash-keep
                #:delete-flash
                #:sweep-flash)
  (:import-from #:foo.lisp.resource
                #:path)
  (:import-from #:foo.lisp.http-response
                #:client-error
                #:server-error
                #:status-code
                #:status-code-error)
  (:import-from #:foo.lisp.params
                #:get-param
                #:get-nested-param
                #:collect-params
                #:collect-nested-params)
  (:import-from #:foo.lisp.vinland/params
                #:validate-params)
  (:import-from #:foo.lisp.vinland/handler/simple
                #:define-controller)
  (:import-from #:foo.lisp.vinland/web
                #:route-url
                #:route-path
                #:halt
                #:binding
                #:body-params
                #:query-params
                #:get-body-param
                #:get-query-param
                #:collect-body-params
                #:collect-query-params
                #:html-safe
                #:negotiate
                #:render
                #:respond
                #:redirect
                #:redirect-back
                #:set-response-status
                #:set-response-headers
                #:cookie
                #:set-cookies
                #:delete-cookie
                #:set-session-options
                #:session
                #:set-session
                #:clear-session)
  (:import-from #:todo-app/user
                #:current-user
                #:require-login
                #:require-no-login
                #:redirect-to-sign-in)
  (:documentation "Package containing controllers."))

(defpackage #:todo-app/params
  (:use #:cl)
  (:import-from #:safety-params
                #:validation-error
                #:invalid-keys
                #:missing-keys
                #:unpermitted-keys
                #:validate
                #:alist
                #:requires
                #:satisfies)
  (:import-from #:foo.lisp.vinland/params
                #:%validate-params)
  (:import-from #:foo.lisp.params
                #:get-param
                #:get-nested-param
                #:collect-params
                #:collect-nested-params)
  (:documentation "Package containing generic functions used to validate request query or body parameters."))

(defpackage #:todo-app/web
  (:use #:cl #:todo-app/controller)
  (:export #:*router*
           #:*web*)
  (:documentation "Package containing special variables required to start the application."))

(defpackage #:todo-app/app
  (:use #:cl)
  (:export #:*app*)
  (:documentation "Package containing *APP*, the Clack application *WEB* wrapped in a middleware pipeline."))

(defpackage #:todo-app/cli
  (:use #:cl)
  (:export #:main)
  (:documentation "Program entrypoint package."))

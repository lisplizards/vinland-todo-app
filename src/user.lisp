;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/user)

(declaim (ftype (function (list) (or null todo-app/dao:user)) current-user))
(defun current-user (env)
  "Loads the current user from session data; called by the *LACK-MIDDLEWARE-USER* middleware,
which dynamically binds the result to *CURRENT-USER*."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list env))
  (let ((session-user-data (or (gethash :user (getf env :lack.session))
                               (return-from current-user))))
    (trivia:ematch
     (todo-app/store:find-user-by-id
      (or (gethash :id session-user-data)
          (return-from current-user)))
     ((list :ok (list :user user))
      (declare (type todo-app/dao:user user))
      user)
     ((list :error :not-found)
      nil))))

(declaim (ftype (function () null) require-login))
(defun require-login ()
  "Redirects to the sign-in page unless *CURRENT-USER* is (dynamically) bound. Ensures that
certain pages should only be accessible to signed-in users."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless *current-user*
    (redirect-to-sign-in)
    (halt)))

(declaim (ftype (function () null) require-no-login))
(defun require-no-login ()
  "Redirects to the lists page unless *CURRENT-USER* is bound. Used to redirect from specific
pages like login and registration that should not be accessible to signed-in users."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((response-code (case (lack.request:request-method vinland:*request*)
                         (:GET 302)
                         (:HEAD 302)
                         (t 303))))
    (declare (type integer response-code))
    (when *current-user*
      (halt
       (redirect "/lists" :status response-code)))))

(declaim (ftype (function () null) redirect-to-sign-in))
(defun redirect-to-sign-in ()
  "Redirects to the sign-in page; checks the HTTP request method and replies with HTTP response
status code 302 if the request is safe (HEAD or GET), otherwise replies with a 303 response so
that the browser does not follow the redirect using the same (unsafe) method from the original
request. Stores the original URL in the session."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (foo.lisp.vinland/web:set-session :origin (lack.request:request-path-info vinland:*request*))
  (redirect "/login"
            :flash '(:notice "Sign in to continue")
            :status (the foo.lisp.http-response:status-code-redirect
                         (case (lack.request:request-method vinland:*request*)
                           (:GET 302)
                           (:HEAD 302)
                           (t 303))))
  (values))


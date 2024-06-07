;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/app)

(defparameter *app*
  (lack:builder
   (:static :path (lambda (path)
                    (if (ppcre:scan "^(?:/images/|/css/|/js/|.png$|/robot\\.txt$|/favicon\\.ico$)" path)
                        path
                        nil))
            :root todo-app/config:*static-directory*)
   :request-id
   (:redact :parameters '("username" "password" "token")
            :preserve-cookies '("_sid"))
   (:http-methods :methods '(:HEAD :GET :POST :PUT :PATCH :DELETE :OPTIONS))
   (:security-headers
    :x-frame-options "DENY"
    :x-xss-protection "0"
    :x-content-type-options "nosniff"
    :x-permitted-cross-domain-policies "none"
    :referrer-policy "strict-origin-when-cross-origin"
    :content-security-policy "default-src 'self' data: https://cdn.jsdelivr.net/; script-src 'self' https://cdn.jsdelivr.net/ 'unsafe-inline'; img-src 'self' https://cdn.jsdelivr.net/; style-src 'self' https://cdn.jsdelivr.net/ 'unsafe-inline'; font-src 'self' https://cdn.jsdelivr.net/"
    :permissions-policy "accelerometer=(), ambient-light-sensor=(), autoplay=(), battery=(), camera=(), display-capture=(), document-domain=(), encrypted-media=(), execution-while-out-of-viewport=(), fullscreen=(), gamepad=(), geolocation=(self), gyroscope=(), hid=(), identity-credentials-get=(), idle-detection=(), local-fonts=(), magnetometer=(), microphone=(), midi=(), otp-credentials=(self), payment=(self), picture-in-picture=(), publickey-credentials-create=(), screen-wake-lock=(), serial=(), speaker-selection=(), storage-access=(), usb=(), web-share=(), window-management=(), xr-spatial-tracking=()")
   (:charset :default '(("text/html" . "utf-8")))
   :head
   (lambda (app)
     (declare (type function app))
     (lambda (env)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type list env))
       (if (eq :OPTIONS (getf env :request-method))
           (let ((response (funcall app env)))
             (declare (type list response))
             (unless (getf (second response) :cache-control)
               (rplacd (last (second response)) '(:cache-control "max-age=3600")))
             response)
           (funcall app env))))
   (:redis :pools '((:pool-id :page-visits
                     :host "localhost"
                     :max-open-count 10
                     :max-idle-count 4)))
   (:session :store (lack/middleware/session/store/redis-pool:make-redis-store
                     :host "localhost"
                     :max-open-count 4
                     :max-idle-count 2)
             :state (lack/session/state/cookie:make-cookie-state
                     :cookie-key "_sid"
                     :path "/"
                     :domain "localhost"
                     :expires 3600
                     :httponly t
                     :secure nil
                     :samesite :strict))
   (:user :current (lambda (env)
                     (declare (type list env))
                     (todo-app/user:current-user env)))
   (:csrf :block-app (lambda (env)
                       (declare (ignore env)
                                (type list env))
                       (error 'foo.lisp.http-response:client-error :status-code 422)))
   :flash
   (lambda (app)
     (declare (type function app))
     (let ((scanner (ppcre:create-scanner "text/html")))
       (declare (type function scanner))
       (lambda (env)
         (declare (optimize (speed 3) (safety 0) (debug 0))
                  (type list env))
         (let ((response (funcall app env)))
           (declare (type list response))
           (when (and (eq :GET (getf env :request-method))
                      (ppcre:scan scanner (getf (second response) :content-type)))
             (lack/middleware/redis:with-redis (:page-visits)
               (red:incr "vinland-todo-app:hits")))
           response))))
   (:errors :app (foo.lisp.vinland/errors-app/simple/dynamic-override:make-app
                  :root todo-app/config:*static-errors-directory*
                  :required-static-response-codes todo-app/http-error:*required-handlers*
                  :required-handler-response-codes todo-app/http-error:*required-handlers*
                  :static-file-types todo-app/http-error:*static-file-types*
                  :handlers todo-app/http-error:*http-errors*
                  :dynamic-override-p (lambda (env)
                                        (declare (ignore env))
                                        lack/middleware/user:*current-user*))
            :intercept (lambda (condition)
                         (declare (type error condition))
                         (typecase condition
                           (foo.lisp.http-response:http-error (slot-value
                                                               condition
                                                               'foo.lisp.http-response:status-code))
                           (foo.lisp.raven:no-route-error 404)
                           (lack/middleware/session/store/redis-pool:redis-pool-timeout-error 503))))
   (:debug :print-env t
           :print-backtrace t
           :special-variables '(foo.lisp.vinland:*route*
                                foo.lisp.vinland:*binding*
                                todo-app/config:*rucksack-directory*)
           :include-special-variables-html t
           :include-backtrace-html t)
   todo-app/web:*web*))

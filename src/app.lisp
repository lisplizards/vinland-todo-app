;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/app)

(defparameter *app*
  (lack:builder
   (:http-methods :methods '(:HEAD :GET :POST :PUT :PATCH :DELETE :OPTIONS))
   :head
   (:security-headers
    :x-frame-options "DENY"
    :x-xss-protection "0"
    :x-content-type-options "nosniff"
    :x-permitted-cross-domain-policies "none"
    :referrer-policy "strict-origin-when-cross-origin"
    :content-security-policy "default-src 'self' data: https://cdn.jsdelivr.net/; script-src 'self' https://cdn.jsdelivr.net/ 'unsafe-inline'; img-src 'self' https://cdn.jsdelivr.net/; style-src 'self' https://cdn.jsdelivr.net/ 'unsafe-inline'; font-src 'self' https://cdn.jsdelivr.net/"
    :permissions-policy "accelerometer=(), ambient-light-sensor=(), autoplay=(), battery=(), camera=(), display-capture=(), document-domain=(), encrypted-media=(), execution-while-out-of-viewport=(), fullscreen=(), gamepad=(), geolocation=(self), gyroscope=(), hid=(), identity-credentials-get=(), idle-detection=(), local-fonts=(), magnetometer=(), microphone=(), midi=(), otp-credentials=(self), payment=(self), picture-in-picture=(), publickey-credentials-create=(), screen-wake-lock=(), serial=(), speaker-selection=(), storage-access=(), usb=(), web-share=(), window-management=(), xr-spatial-tracking=()")
   (:charset :default '(("text/html" . "utf-8")
                        ("application/json" . "utf-8")))
   (:static :path (lambda (path)
                    (if (ppcre:scan "^(?:/images/|/css/|/js/|.png$|/robot\\.txt$|/favicon\\.ico$)" path)
                        path
                        nil))
            :root todo-app/config:*static-directory*)
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
   (:session :store (lack/session/store/memory:make-memory-store)
             :state (lack/session/state/cookie:make-cookie-state
                     :cookie-key "_sid"
                     :path "/"
                     :domain "localhost"
                     :expires (+ 600 (get-universal-time))
                     :httponly t
                     :secure nil
                     :samesite :strict))
   :csrf
   :flash
   (:user :current (lambda (env)
                     (todo-app/user:current-user env)))
   (:backtrace
    :result-on-error `(500
                       (:content-type "text/plain"
                        :content-length 21)
                       ("Internal Server Error")))
   todo-app/web:*web*))

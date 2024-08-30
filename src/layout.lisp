;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/layout)

(defparameter *importmap*
  (foo.lisp.vinland/web:importmap
   '(("@shoelace-style/form" . "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.0/cdn/utilities/form.js")
     ("@hotwired/turbo" . "/js/vendor/turbo.es2017-esm.js")
     ("@hotwired/stimulus" . "/js/vendor/stimulus.js"))))

(defmacro with-main-layout ((&key title
                               links
                               scripts
                               skip-main-js
                               main-container-class)
                            &body body)
  `(spinneret:with-html-string (:doctype)
     (:html
      (:head
       (:meta :name "charset" :content "UTF-8")
       (:meta :name "viewport" :content "width=device-width,initial-scale=1")
       (:meta :name "turbo-refresh-method" :content "morph")
       (:title ,title)
       (:link :rel "stylesheet" :href "/css/vendor/shoelace/theme-light.css")
       (:link :rel "stylesheet" :type "text/css" :href "/css/main.css")
       ,@(when links
           (loop for link in links
                 collect link))
       (:script :type "importmap" (:raw ,*importmap*))
       (:script :type "module"
                :src "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.0/cdn/shoelace-autoloader.js"
                :async t)
       ,(unless skip-main-js
          '(:script :type "module" :src "/js/main.js"))
       ,@(when scripts
           (loop for script in scripts
                 collect script)))
      (:body
       (todo-app/component:site-header)
       (:div :class (format nil "main-container ~A"
                            ,(or main-container-class ""))
             ,@body)
       (todo-app/component:site-footer)))))

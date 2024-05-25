;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:todo-app/view)

(setf spinneret:*html-style* :tree)

(defmacro with-main-layout ((&key title
                               links
                               scripts
                               main-container-class)
                            &body body)
  `(spinneret:with-html-string (:doctype)
     (:html
      (:head
       (:meta :name "charset" :content "UTF-8")
       (:meta :name "viewport" :content "width=device-width,initial-scale=1")
       (:meta :name "turbo-refresh-method" :content "morph")
       (:title ,title)
       (:link :rel "stylesheet"
              :href "/css/vendor/shoelace/theme-light.css")
       (:link :rel "stylesheet"
              :type "text/css"
              :href "/css/main.css")
       ,@(when links
           (loop for link in links
                 collect link))
       (:script :type "importmap"
                (:raw ,(com.inuoe.jzon:stringify
                        (cl-hash-util:hash
                         ("imports"
                          (cl-hash-util:hash
                           ("@shoelace-style/form"
                            "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.0/cdn/utilities/form.js")
                           ("@hotwired/turbo" "/js/vendor/turbo.es2017-esm.js")
                           ("@hotwired/stimulus" "/js/vendor/stimulus.js"))))
                        :stream nil
                        :pretty t)))
       (:script :type "module" :src "/js/main.js")
       (:script :type "module"
                :src "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.0/cdn/shoelace-autoloader.js")
       ,@(when scripts
           (loop for script in scripts
                 collect script)))
      (:body
       (todo-app/component:site-header)
       (:div :class (format nil "main-container ~A"
                            ,(or main-container-class ""))
             ,@body)
       (todo-app/component:site-footer)))))

(defun about (&key (page-hits 0))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type integer page-hits))
  (with-main-layout
      (:title "About"
       :main-container-class "center")
    (:div
     (:h1 "About")
     (:p (:em "To Do app")
         " is a demonstration of the Vinland web framework."))
    (:p (format nil "Total page hits: ~D" page-hits))
    (:img :src "/images/lisp-lizard-with-text.svg" :alt "Lisp Lizard")))

(defun 🧐 ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-main-layout
    (:title "🧐"
     :main-container-class "center")
    (:div
     (:h1 "🧐")
     (:p "光环世界")
     (:p "Привет, мир")
     (:p "안녕 세계")
     (:p "สวัสดีชาวโลก")
     (:p "こんにちは世界")
     (:p "مرحبا بالعالم")
     (:p "سلام دنیا")
     (:p "Γειά σου Κόσμε"))))

(defun login ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-main-layout
      (:title "Sign in"
       :main-container-class "center expand-horizontal")
    (:h1 :class "login-title" "Sign in")
    (todo-app/component:login-form-box)))

(defun register ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-main-layout
      (:title "Register"
       :main-container-class "center expand-horizontal")
    (:h1 :class "registration-title" "Register")
    (todo-app/component:registration-form-box)))

(defun todo-lists (&key todo-lists)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-main-layout
      (:title "Lists"
       :main-container-class "block")
    (:turbo-frame :class "new-todo-list-form-box" :id "new-todo-list-form-box"
                  (todo-app/component:new-todo-list-form))
    (:div :class "todo-lists-box"
     (:h1 "Lists")
     (todo-app/component:todo-list-links :todo-lists todo-lists))))

(defun todo-list (&key todo-list todo-items)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type todo-app/rucksack:todo-list todo-list)
           (type list todo-items))
  (with-main-layout
      (:title "To Do list"
       :main-container-class "block")
    (:div :class "todo-list-box"
          (:h1 (todo-app/rucksack:todo-list-title todo-list))
          (todo-app/component:flash-container)
          (todo-app/component:new-todo-item-form :todo-list todo-list)
          (:ul :id "todo-list-items-list"
               :data-controller "todo-list"
               (mapcar
                #'(lambda (todo-item)
                    (declare (type todo-app/rucksack:todo-item todo-item))
                    (todo-app/component:todo-item :todo-item todo-item))
                todo-items)))))

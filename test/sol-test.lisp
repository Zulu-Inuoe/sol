(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((ql:*local-project-directories* ql:*local-project-directories*))
    (uiop:with-current-directory ((uiop:pathname-directory-pathname (or *load-truename* *compile-file-truename*)))
      (pushnew (truename "../") ql:*local-project-directories*))
    (ql:register-local-projects)
    (ql:quickload :sol)
    (asdf:compile-system :sol)
    (asdf:load-system :sol)))

(defpackage #:sol-test
  (:use #:cl #:sol)
  (:local-nicknames
   (#:ui #:sol.ui)))

(in-package #:sol-test)

(defclass test-app (app)
  ())

(defmethod app-init ((app test-app))
  (make-instance
   'ui:window
   :left 2300 :top 300
   :height 350 :width 525
   :content
   (make-instance
    'ui:button
    :content "Hello, world!")))

(defun run-test ()
  (app-start 'test-app))


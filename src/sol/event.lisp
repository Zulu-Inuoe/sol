;;;Copyright (c) 2017 Wilfredo Velázquez-Rodríguez
;;;
;;;This software is provided 'as-is', without any express or implied
;;;warranty. In no event will the authors be held liable for any damages
;;;arising from the use of this software.
;;;
;;;Permission is granted to anyone to use this software for any purpose,
;;;including commercial applications, and to alter it and redistribute
;;;it freely, subject to the following restrictions:
;;;
;;;1. The origin of this software must not be misrepresented; you must not
;;;   claim that you wrote the original software. If you use this software
;;;   in a product, an acknowledgment in the product documentation would
;;;   be appreciated but is not required.
;;;
;;;2. Altered source versions must be plainly marked as such, and must not
;;;   be misrepresented as being the original software.
;;;
;;;3. This notice may not be removed or altered from any source distribution.

(in-package #:sol)

(defclass event ()
  ((name
    :type (or null string) :initform nil
    :initarg :name :reader name)
   (handlers
    :initform nil
    :accessor handlers)))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "~A (~D)" (name event) (length (handlers event)))))

(defgeneric event-subscribe (event obj handler &key priority))
(defgeneric event-notify (event args))
(defgeneric event-unsubscribe (event obj-or-handler))

(defmethod event-subscribe ((event event) obj handler &key (priority 500))
  (if obj
      ;;Close over event
      (setf handler (%close-over obj handler))
      ;;Otherwise we use the handler itself as the key
      (setf obj handler))

  (setf (handlers event)
        (stable-sort (nconc (handlers event)
                            (list (make-instance
                                   '%event-handler
                                   :key obj
                                   :handler handler
                                   :priority priority)))
                     #'<
                     :key #'priority))
  (values))

(defmethod event-notify ((event event) args)
  (dolist (handler (handlers event))
    (funcall (handler handler) args))
  (values))

(defmethod event-unsubscribe ((event event) obj-or-handler)
  (setf (handlers event)
        (delete
         obj-or-handler
         (handlers event)
         :key #'key))
  (values))

(defun event-once (event obj handler &key (priority 500))
  (if obj
      ;;Close over event
      (setf handler (%close-over obj handler))
      ;;Otherwise we use the handler itself as the key
      (setf obj handler))

  (let ((handler
         (lambda (args)
           (funcall handler args)
           (event-unsubscribe event obj))))

    (setf (handlers event)
          (stable-sort (nconc (handlers event)
                              (list (make-instance
                                     '%event-handler
                                     :key obj
                                     :handler handler
                                     :priority priority)))
                       #'<
                       :key #'priority))))

(defun %close-over (obj handler)
  (lambda (args)
    (funcall handler obj args)))

(defclass %event-handler ()
  ((key
    :initarg :key
    :reader key)
   (handler
    :initarg :handler
    :reader handler)
   (priority
    :initarg :priority
    :reader priority)))

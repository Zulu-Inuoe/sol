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

(in-package #:sol.dispatcher)

(defclass %auto-reset-event ()
  ((%name
    :type string
    :reader %ar-event-name)
   %lock
   %cond-var
   (%set-p
    :type boolean
    :initform nil)))

(defmethod initialize-instance :after ((event %auto-reset-event) &key (name "auto-reset-event"))
  (with-slots (%name %lock %cond-var) event
    (setf %name name)
    (setf %lock (bordeaux-threads:make-lock name))
    (setf %cond-var (bordeaux-threads:make-condition-variable :name name))))

(defun %wait-one (event)
  (with-slots (%lock %cond-var %set-p) event
    (bordeaux-threads:with-lock-held (%lock)
      (or %set-p
          (loop :until (bordeaux-threads:condition-wait %cond-var %lock)))
      (setf %set-p nil)
      t)))

(defun %set-event (event)
  (with-slots (%lock %cond-var %set-p) event
    (bordeaux-threads:with-lock-held (%lock)
      (setf %set-p t)
      (bordeaux-threads:condition-notify %cond-var))))

(defclass %simple-dispatcher (dispatcher)
  ((%simple-dispatcher-invoke-event
    :initform
    (make-instance
     '%auto-reset-event
     :name
     (format nil "SimpleDispatcherInvokeEvent[~A(~A);~A]"
             (lisp-implementation-type)
             (lisp-implementation-version)
             (bordeaux-threads:thread-name (bordeaux-threads:current-thread))))
    :reader %simple-dispatcher-invoke-event)))

(defmethod impl:wait-invoke-signal ((dispatcher %simple-dispatcher))
  (and (%wait-one (%simple-dispatcher-invoke-event dispatcher))
       (not (shutdown-started dispatcher))))

(defmethod impl:send-invoke-signal ((dispatcher %simple-dispatcher))
  (%set-event (%simple-dispatcher-invoke-event dispatcher)))

(defmethod impl:send-shutdown-signal ((dispatcher %simple-dispatcher))
  (%set-event (%simple-dispatcher-invoke-event dispatcher)))
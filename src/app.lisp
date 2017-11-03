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

(defclass app (dispatcher:dispatcher-object)
  ((main-window
    :type (or null ui:window)
    :initform nil
    :reader main-window)
   (windows
    :type list
    :initform ()
    :accessor windows)))

(defun (setf main-window) (new-value app)
  (let ((old-value (main-window app)))
    (unless (eq old-value new-value)
      (when old-value
        (event-unsubscribe
         (ui:e_window-closed old-value)
         app))

      (setf (slot-value app 'main-window) new-value)

      (when new-value
        (event-subscribe
         (ui:e_window-closed new-value)
         app
         '%app.on-main-window-closed))))
  (main-window app))

(defvar *%current-app* nil)

(defun current-app ()
  *%current-app*)

(defgeneric app-init (app))
(defgeneric app-uninit (app))

(defmethod app-init ((app app))
  (declare (ignore app))
  (values))

(defmethod app-uninit ((app app))
  (when (main-window app)
    (event-unsubscribe
     (ui:e_window-closed (main-window app))
     app))

  (dolist (w (windows app))
    (ui:window-close w))
  (setf (windows app) nil))

(defun app-start (&optional (app-class 'app))
  (when (current-app)
    (error "app: app already running"))

  (drivers:ensure-active-driver)

  (let ((app (make-instance app-class))
        success)
    (setf *%current-app* app)
    (unwind-protect
         (progn
           (app-init app)
           (setf success t))
      (unless success
        (setf *%current-app* nil)))
    (setf success nil)

    (unwind-protect
         (progn
           ;;run the dispatcher
           (dispatcher:run)
           (setf success t))
      (unless (and success (null *%current-app*))
        ;;When either we encountered an error,
        ;;or the app was not shut down properly
        ;;uninitialize before exiting
        (unwind-protect
             (app-uninit app)
          (setf *%current-app* nil)))))
  (values))

(defun app-quit (&aux (app (current-app)))
  (unless app
    (error "app: no current app running"))
  (dispatcher:verify-access app)

  (unwind-protect
       (app-uninit app)
    (setf *%current-app* nil))

  (dispatcher:invoke-shutdown (dispatcher:dispatcher app))
  ;;Shutting down the dispatcher more or less kills the driver anyway
  (drivers:ensure-shutdown-driver)
  (values))

(defmethod impl:app-add-window ((app app) window)
  (push window (windows app))
  (unless (main-window app)
    (setf (main-window app) window)))

(defun %app.on-main-window-closed (app window)
  (setf (windows app) (delete window (windows app)))

  (when (eq window (main-window app))
    (app-quit)))
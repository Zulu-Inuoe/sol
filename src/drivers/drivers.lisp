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

(in-package #:sol.drivers)

(defvar *%driver-class* nil)

(defun driver-class ()
  *%driver-class*)

(defun (setf driver-class) (new-value)
  (setf *%driver-class* new-value))

(defmacro define-driver (name)
  `(when (null (driver-class))
     (setf (driver-class) ',name)))

(defgeneric driver-dispatcher (driver))

(defvar *%active-driver* nil)

(defun active-driver ()
  *%active-driver*)

(defun ensure-active-driver ()
  (unless (active-driver)
    (let ((driver-class (driver-class)))
      (unless driver-class
        (error "sol.drivers: no driver class available"))

      (setf *%active-driver* (make-instance driver-class))))

  (active-driver))

(defun ensure-shutdown-driver ()
  (when (active-driver)
    (dispose (active-driver))
    (setf *%active-driver* nil)))

;;;Font
(define-property font-impl)

(defgeneric font-height (font-impl))
(defgeneric font-size-text (font-impl text))

;;;Text
(define-property text-impl)

(defgeneric text-width (text-impl))
(defgeneric text-height (text-impl))
(defgeneric text-set-dirty (text-impl))

;;;image
(define-property image-impl)

(defgeneric image-width (image-impl))
(defgeneric image-height (image-impl))
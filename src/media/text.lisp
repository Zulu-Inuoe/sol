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

(in-package #:sol.media)

(defclass text ()
  ((text-string
    :type string
    :initarg :string
    :initform ""
    :reader text-string)
   (text-font
    :type font
    :initarg :font
    :initform (make-instance 'font)
    :reader text-font)
   (text-color
    :type color
    :initarg :color
    :initform colors:*black*
    :reader text-color)
   (impl
    :reader text-impl)))

(defmethod initialize-instance :after ((text text) &key &allow-other-keys)
  (drivers:ensure-active-driver)
  (setf (slot-value text 'impl)
        (make-instance (drivers:driver-text-impl (drivers:active-driver)) :text text)))

(defmethod print-object ((text text) stream)
  (print-unreadable-object (text stream :type t)
    (format stream "\"~A\"" (text-string text))))

(defun (setf text-string) (value text)
  (when (string-not-equal value (text-string text))
    (setf (slot-value text 'text-string) value)
    (drivers:text-set-dirty (text-impl text)))
  value)

(defun (setf text-font) (value text)
  (setf (slot-value text 'text-font) value)
  (drivers:text-set-dirty (text-impl text))
  value)

(defun (setf text-color) (value text)
  (when (not (color= value (text-color text)))
    (setf (slot-value text 'text-color) value)
    (drivers:text-set-dirty (text-impl text)))
  value)

(defun text-width (text)
  (drivers:text-width (text-impl text)))

(defun text-height (text)
  (drivers:text-height (text-impl text)))
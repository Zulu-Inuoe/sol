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

(in-package #:sol.input)

(defclass input-manager ()
  ((e_mouse-move
    :type event
    :initform (make-instance 'event :name "mouse-move")
    :reader e_mouse-move)
   (e_mouse-button
    :type event
    :initform (make-instance 'event :name "mouse-button")
    :reader e_mouse-button)
   (e_mouse-wheel
    :type event
    :initform (make-instance 'event :name "mouse-wheel")
    :reader e_mouse-wheel)
   (e_key
    :type event
    :initform (make-instance 'event :name "key")
    :reader e_key)
   (e_text-input
    :type event
    :initform (make-instance 'event :name "text-input")
    :reader e_text-input)
   (e_controller-button
    :type event
    :initform (make-instance 'event :name "controller-button")
    :reader e_controller-button)
   (e_controller-axis
    :type event
    :initform (make-instance 'event :name "controller-axis")
    :reader e_controller-axis)))

(defmethod initialize-instance :after ((input input-manager) &key &allow-other-keys)
  (setf (gethash (bordeaux-threads:current-thread) *%input-managers*) input))

(defmethod dispose ((obj input-manager))
  (loop
     :for (thread . manager) :in (hash-table-alist *%input-managers*)
     :when (eq obj manager)
     :do (remhash thread *%input-managers*))
  (call-next-method))

(defvar *%input-managers* (make-hash-table)
  "A mapping of thread-id's to `input-manager's.
   This holds all currently existing input-managers.")

(defun from-thread (thread)
  "Gets the `input-manager' for the given thread, or NIL if no such `input-manager' exists."
  (values (gethash thread *%input-managers*)))

(defun current-input-manager ()
  "Gets the `input-manager' belonging to the current thread."
  (or (from-thread (bordeaux-threads:current-thread))
      (make-instance 'input-manager)))
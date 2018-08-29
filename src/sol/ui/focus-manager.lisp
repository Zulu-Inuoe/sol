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

(in-package #:sol.ui)

(defparameter *active-focus-manager* nil)

(defun active-focus-manager ()
  *active-focus-manager*)

(defun (setf active-focus-manager) (new-val)
  (let ((prev-val (active-focus-manager)))
    (when (not (eq prev-val new-val))
      (setf *active-focus-manager* new-val)

      (let ((prev-comp (and prev-val
                            (focused-component prev-val)))
            (new-comp (and new-val
                           (focused-component new-val))))
        (when prev-comp
          (raise-event
           prev-comp
           'e_lost-focus
           (make-instance
            'focus-event-args
            :source prev-comp
            :opposite-component new-comp)))

        (when new-comp
          (raise-event
           new-comp
           'e_got-focus
           (make-instance
            'focus-event-args
            :source new-comp
            :opposite-component prev-comp))))))

  new-val)

(defgeneric focused-component (focus-manager))
(defgeneric (setf focused-component) (new-value focus-manager))

(defmethod focused-component ((focus-manager null))
  nil)

(defmethod (setf focused-component) (new-val (focus-manager null))
  nil)

(defclass focus-manager ()
  ((%focused-component
    :type (or null component)
    :initform nil)))

(defmethod focused-component ((focus-manager focus-manager))
  (slot-value focus-manager '%focused-component))

(defmethod (setf focused-component) (new-val (focus-manager focus-manager))
  (let ((prev-val
         (slot-value focus-manager '%focused-component)))
    (when (not (eq prev-val new-val))
      (setf (slot-value focus-manager '%focused-component) new-val)
      (when prev-val
        (raise-event
         prev-val
         'e_lost-focus
         (make-instance
          'focus-event-args
          :source prev-val
          :opposite-component new-val)))

      (when new-val
        (raise-event
         new-val
         'e_got-focus
         (make-instance
          'focus-event-args
          :source new-val
          :opposite-component prev-val)))))
  new-val)

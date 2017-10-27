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

(cl:in-package #:sol.ui)

(defclass panel (component)
  ((children
    :type list :initform nil
    :reader children))
  (:default-initargs
   :background media.colors:*transparent*))

(defgeneric add-child (panel child))
(defgeneric remove-child (panel child))

(defmethod initialize-instance :after ((comp panel)
                                       &key children
                                         &allow-other-keys)
  (dolist (child children)
    (add-child comp child)))

(defmethod add-child ((comp panel) (child component))
  (setf (parent child) comp)
  (with-slots (children) comp
    (setf children (nconc children (list child))))
  (when (is-loaded comp)
    (load-component child))
  child)

(defmethod remove-child ((comp panel) (child component))
  (assert (eq (parent child) comp) ())

  (when-let ((focused-component (focused-component (active-focus-manager))))
    (when (or
           (eq focused-component child)
           (has-parent-p focused-component child))
      (setf (focused-component (active-focus-manager)) nil)))

  (when (is-loaded comp)
    (unload-component child))

  (setf (parent child) nil
        (slot-value comp 'children) (delete child (children comp)))
  (values))


(defmethod get-component-at-* ((comp panel) x y)
  (if (visible comp)
      (loop
         :for child :in (reverse (children comp))
         :for child-offset-x := (x child)
         :for child-offset-y := (y child)
         :do
         (multiple-value-bind (ret ret-x ret-y)
             (get-component-at-* child (- x child-offset-x) (- y child-offset-y))
           (when ret
             (return (values ret ret-x ret-y))))
         :finally
         (return (call-next-method)))
      nil))

(defmethod load-component ((comp panel))
  (dolist (child (children comp))
    (load-component child)))

(defmethod unload-component ((comp panel))
  (dolist (child (children comp))
    (unload-component child)))

(defmethod draw ((comp panel) renderer)
  (dolist (child (children comp))
    (draw child)))
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

(in-package #:sol.core)

(defgeneric dispose (object)
  (:documentation
   "Interface for disposing an object. This cleans up any resources used by said object.
No return value.")
  (:method (object)
    (declare (ignore object))
    (values)))

(defmethod dispose :around (object)
  (call-next-method))

(defmacro with-disposeable ((var val) &body body)
  "'var' is bound to the value of 'val', and then 'body' is executed as an implicit progn.
val is automatically disposed on exit from with-disposable."
  `(let ((,var ,val))
     (unwind-protect
          (progn ,@body)
       (dispose ,var))))

(defclass disposeable ()
  ((disposed
    :type boolean
    :initform nil
    :accessor disposed
    :documentation "Boolean indicating whether the object has been disposed."))
  (:documentation "Mixin that prevents being disposed multiple times."))

(defmethod dispose :around ((obj disposeable))
  "Calls dispose on 'obj' if it has not already been disposed."
  (unless (disposed obj)
    (setf (disposed obj) t)
    (call-next-method))
  (values))
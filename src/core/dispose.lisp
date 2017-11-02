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

(defmacro dispose-on-error (&body body)
  "Executes each form in body as an implicit progn.
If any form signals an error, the values of all previously evaluated forms are `dispose'd, in lifo order.
Note, even if a `dispose' on a subsequent form fails, `dispose' will be called on all forms."
  (labels ((recurse (forms)
             (with-gensyms (success-p value-sym)
                  `(let (,success-p ,value-sym)
                     (unwind-protect
                       ,(cond
                          ((null (cdr forms))
                           `(prog1 (setf ,value-sym ,(car forms))
                              (setf ,success-p t)))
                          (t
                           `(progn
                              (setf ,value-sym ,(car forms))
                              (prog1 ,(recurse (cdr forms))
                                (setf ,success-p t)))))
                       (unless ,success-p
                         (when ,value-sym
                           (dispose ,value-sym))))))))
    (if (null body)
        '()
        (recurse body))))

(defmacro ensure-dispose (&body body)
  "Disposes each form in body.
Forms signalling an error will not prevent following forms from being disposed."
  (labels ((recurse (forms)
             (cond
               ((null (cdr forms))
                `(dispose ,(car forms)))
               (t
                `(unwind-protect
                      (dispose ,(car forms))
                   ,(recurse (cdr forms)))))))
    (if (null body)
        '()
        (recurse body))))

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
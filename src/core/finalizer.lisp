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

(defgeneric finalizer (object)
  (:documentation
   "Create a finalizer for the object.
A finalizer is a function of no arguments that cleans up any resources used by 'object'.
The finalizer should not hold a reference to 'object', and may run as part of the garbage collector.
See 'trivial-garbage:finalize'.")
  (:method (object)
    (declare (ignore object))
    (lambda ())))

(defmacro define-finalizer (class (&rest slots) &body body)
  "Defines a 'finalizer' method for 'class' in an which the slot-values values of 'slots' are bound to their respective names.
Ex:

(define-finalizer net-logger (sock log-stream)
  (usocket:socket-close sock)
  (close log-stream))"
  (with-gensyms (obj parent-fn)
    `(defmethod finalizer ((,obj ,class))
       (let ((,parent-fn (call-next-method))
             ,@(mapcar
                (lambda (slot-name)
                  `(,slot-name (slot-value ,obj ',slot-name)))
                slots))
         (lambda ()
           (unwind-protect
                (progn
                  ,@body)
             (funcall ,parent-fn)))))))

(defclass finalizable (disposeable)
  ()
  (:documentation
   "Mixin to establish reasonable dispose and finalize methods.
`finalizable' objects establish a finalizer for themselves via 'trivial-garbage:finalize', utilizing 'finalizer'.
A 'dispose' implementation is provided that invokes 'finalizer', and cancels finalization via 'trivial-garbage:cancel-finalization'."))

(defmethod initialize-instance :around ((obj finalizable) &key &allow-other-keys)
  "Registers the object"
  (call-next-method)
  (trivial-garbage:finalize obj (finalizer obj)))

(defmethod dispose ((obj finalizable))
  "Cancels finalization on 'obj' and invokes its finalizer."
  (trivial-garbage:cancel-finalization obj)
  (funcall (finalizer obj))
  (values))
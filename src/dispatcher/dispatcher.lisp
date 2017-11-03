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

(defconstant +priority.inactive+ 0)
(defconstant +priority.system-idle+ 1)
(defconstant +priority.application-idle+ 2)
(defconstant +priority.context-idle+ 3)
(defconstant +priority.background+ 4)
(defconstant +priority.input+ 5)
(defconstant +priority.loaded+ 6)
(defconstant +priority.render+ 7)
(defconstant +priority.databind+ 8)
(defconstant +priority.normal+ 9)
(defconstant +priority.send+ 10)

(defgeneric check-access (obj)
  (:documentation "Returns T if the object OBJ is accessible from the current thread."))

(defgeneric verify-access (obj)
  (:documentation "Signals an error if OBJ is inaccessible from the current thread.")
  (:method (obj)
    (unless (check-access obj)
      (error "Incorrect thread."))
    (values)))

(defconstant +%max-parallel-invoke+ 10
  "The maximum number of parallel INVOKE calls that may occur. Subsequent calls will wait until a previous call has completed.
   Calling INVOKE from the owning thread of a `dispatcher' is reentrant. In practice, this number signifies the number of
   threads that may INVOKE on a single `dispatcher' at a time.")

(defclass dispatcher ()
  ((queue
    :type cl-heap:priority-queue
    :initform (make-instance 'cl-heap:priority-queue)
    :reader queue)
   (queue-lock
    :initform
    (bordeaux-threads:make-lock
     (format nil "DispatcherQueueLock[~A(~A);~A]"
             (lisp-implementation-type)
             (lisp-implementation-version)
             (bordeaux-threads:thread-name (bordeaux-threads:current-thread))))
    :reader queue-lock)
   (sync-events
    :type list
    :initform
    (loop
       :for n :from 0 :below +%max-parallel-invoke+
       :for name := (format nil "DispatcherSyncEvent[~A(~A);~A;~A]"
                            (lisp-implementation-type)
                            (lisp-implementation-version)
                            (bordeaux-threads:thread-name (bordeaux-threads:current-thread))
                            n)
       :collect
       (cons
        (bordeaux-threads:make-lock name)
        (bordeaux-threads:make-condition-variable :name name)))
    :accessor sync-events)
   (sync-lock
    :initform (bordeaux-threads:make-lock)
    :reader sync-lock)
   (%dispatcher-frames
    :type list
    :initform nil
    :accessor %dispatcher-frames)
   (%exit-all-frames
    :type boolean
    :initform nil
    :accessor %exit-all-frames)
   (shutdown-started
    :type boolean
    :initform nil
    :accessor shutdown-started)
   (shutdown-finished
    :type boolean
    :initform nil
    :accessor shutdown-finished)
   (e_unhandled-error
    :type event
    :initform (make-instance 'event :name "unhandled-error")
    :accessor e_unhandled-error)))

(defmethod initialize-instance :after ((obj dispatcher) &key &allow-other-keys)
  (setf (gethash (bordeaux-threads:current-thread) *%dispatchers*) obj))

(defvar *%dispatchers* (make-hash-table)
  "A mapping of thread-id's to `dispatcher's.
   This holds all currently existing dispatchers.")

(defmethod dispose ((obj dispatcher))
  (unless (or (shutdown-started obj)
              (shutdown-finished obj))
    (invoke-shutdown obj))

  (loop
     :for (thread . disp) :in (hash-table-alist *%dispatchers*)
     :when (eq obj disp)
     :do (remhash thread *%dispatchers*))
  (call-next-method))

(defun from-thread (thread)
  "Gets the `dispatcher' for the given thread, or NIL if no such `dispatcher' exists."
  (values (gethash thread *%dispatchers*)))

(declaim (ftype (function () dispatcher) current-dispatcher))

(defmethod check-access ((dispatcher dispatcher))
  "Returns true if the current thread has access to the given `dispatcher'."
  (eq dispatcher (from-thread (bordeaux-threads:current-thread))))

(defclass dispatcher-unhandled-error-args ()
  ((dispatcher
    :type dispatcher
    :initform (current-dispatcher)
    :reader dispatcher)
   (args-error
    :type error
    :initform (error "Must set args error")
    :initarg :error
    :reader args-error)
   (handled
    :type boolean
    :initform nil
    :accessor handled))
  (:documentation
   "The argument type for an unhandled dispatcher error.
    DISPATCHER is the `dispatcher' which produced the error.
    ARGS-ERROR is the `cl:error' produced.
    HANDLED allows event subscribers to specify whether the error has been handled or not."))

(defclass dispatcher-object ()
  ((dispatcher
    :type dispatcher
    :initform (current-dispatcher)
    :reader dispatcher))
  (:documentation
   "Represents an object which is tied to a dispatcher.
Methods specialized on `dispatcher-object' should use the VERIFY-ACCESS function
in order to verify that said operations are legal."))

(defmethod check-access ((dispatcher-object dispatcher-object))
  "Returns T if the current thread has access to the given `dispatcher-object'."
  (check-access (dispatcher dispatcher-object)))

(defclass dispatcher-frame (dispatcher-object)
  ((continue-frame
    :type boolean
    :initform t
    :initarg :continue
    :accessor continue-frame))
  (:documentation
   "Represents a frame of execution on a `dispatcher'."))

(defun push-frame (frame)
  "Pushes a new frame of execution unto the CURRENT-DISPATCHER."
  (let* ((d (dispatcher frame)))
    (push frame (%dispatcher-frames d))

    (unwind-protect
         (loop
            :while (and (not (%exit-all-frames d))
                        (not (shutdown-started d))
                        (continue-frame frame))
            :if (impl:wait-invoke-signal d)
            :do (impl:process-queue d))
      (pop (%dispatcher-frames d)))

    ;;Are we the last frame on the stack?
    (when (null (%dispatcher-frames d))
      ;;Then nil-out the exit-all-frames, in case that was the reason we exited
      (setf (%exit-all-frames d) nil)
      ;;If we started a shutdown
      (when (shutdown-started d)
        ;;The stack has unwound out, so we're done shutting down
        (setf (shutdown-finished d) t))))
  (values))

(defun exit-all-frames ()
  "Exits from all currently active `dispatcher:dispatcher-frame's on the CURRENT-DISPATCHER, but does not shut down the `dispatcher'."
  (setf (%exit-all-frames (current-dispatcher)) t)
  (values))

(defun run ()
  "Runs the CURRENT-DISPATCHER's main execution frame.
   The `dispatcher' will process its event queue in a loop, and will continue doing so
   until it is shutdown, or EXIT-ALL-FRAMES is called."
  (let ((frame (make-instance 'dispatcher-frame)))
    (push-frame frame))
  (values))

(defun begin-invoke (dispatcher fn &key (priority +priority.normal+))
  "Request that the thread owning DISPATCHER invoke FN, asynchronously."
  (%begin-invoke-impl dispatcher fn priority)
  (values))

(defun begin-invoke-shutdown (dispatcher)
  "Request that the specified `dispatcher' is shutdown, asynchronously returning."
  (begin-invoke dispatcher #'%shutdown-callback-impl)
  (values))

(defun invoke (dispatcher fn &key (priority +priority.normal+)
               &aux (ret-value nil))
  "Request the thread owning DISPATCHER to invoke FN synchronously, returning the result."
  (cond
    ;;Did we ask to invoke it on ourselves with the send priority?
    ((and (check-access dispatcher)
          (= priority +priority.send+))
     ;;Invoke it straight away..
     (setf ret-value (%process-invoke dispatcher fn)))
    ;;Did we ask to invoke it on ourselves?
    ((check-access dispatcher)
     ;;Create a new dispatcher frame which will run until
     ;;hook-fn is invoked.
     (let* ((frame (make-instance 'dispatcher-frame))
            (hook-fn (lambda ()
                       (setf (continue-frame frame) nil)
                       ;;If an error is signalled from here no worries because we already set to exit the frame
                       (setf ret-value (funcall fn)))))
       ;;Enqueue our message and post
       (%begin-invoke-impl dispatcher hook-fn priority)

       ;;Push the frame (We'll continue to process stuff until hook-fn is invoked)
       (push-frame frame)))
    (t
     (let (event
          lock
           condition
           (invoke-incomplete t))
       ;;Wait until there's a free event by re-trying to grab it
       ;;Grab an event that we can wait on
       (tagbody
        :try-again
          (bordeaux-threads:with-lock-held ((sync-lock dispatcher))
            ;;Are there any events left?
            (when (null (sync-events dispatcher))
              ;;Release sync-lock and try again
              (go :try-again))
            ;;Grab it
            (setf event (pop (sync-events dispatcher)))))

       (setf lock (car event))
       (setf condition (cdr event))

       ;;Hook-fn here will signal the event when it's called
       (let ((hook-fn (lambda ()
                        (unwind-protect
                             (setf ret-value (funcall fn))
                          (progn
                            (setf invoke-incomplete nil)
                            (bordeaux-threads:condition-notify condition))))))
         ;;Enqueue our message and post
         (%begin-invoke-impl dispatcher hook-fn priority))

       ;;Wait for the event to get signalled from our hook-fn
       (loop :while invoke-incomplete
          :do
          (bordeaux-threads:with-lock-held (lock)
            (bordeaux-threads:condition-wait condition lock)))
       ;;Then put it back some other people can use it
       (bordeaux-threads:with-lock-held ((sync-lock dispatcher))
         (push event (sync-events dispatcher))))))
  ret-value)

(defun invoke-shutdown (dispatcher)
  "Request that the specified `dispatcher' is shutdown, and waits for it to do so."
  (invoke dispatcher #'%shutdown-callback-impl))

(defmacro do-invoke ((dispatcher) &body body)
  `(invoke ,dispatcher (lambda () ,@body)))

(defmacro do-begin-invoke ((dispatcher) &body body)
  `(begin-invoke ,dispatcher (lambda () ,@body)))

(defun impl:process-queue (dispatcher)
  "Processes one invoke item from the `dispatcher''s queue."
  (let (fn)
    (bordeaux-threads:with-lock-held ((queue-lock dispatcher))
      (setf fn (cl-heap:dequeue (queue dispatcher))))
    (when fn
      (%process-invoke dispatcher fn)))
  (values))

(defun %handle-dispatcher-error (d e)
  "Raises the E_UNHANDLED-ERROR event on the `dispatcher' D with the `cl:error' E.
   The event is raised with a fresh `dispatcher-unhandled-error-args', returning T if the error was handled, NIL otherwise."
  (let ((args (make-instance 'dispatcher-unhandled-error-args :error e)))
    (event-notify (e_unhandled-error d) args)
    (and (handled args)
         t)))

(defun %process-invoke (dispatcher fn)
  "Invokes FN with the appropriate handlers and restarts for the given `dispatcher'."
  (restart-case
      (block nil
        (handler-bind ((error (lambda (e)
                                (when (%handle-dispatcher-error dispatcher e)
                                  (return)))))
          (funcall fn)))
    (ignore-dispatcher-error ()
      :report (lambda (stream)
                (format stream "Ignore the error signalled on `dispatcher' ~A and continue." dispatcher)))))

(defun %begin-invoke-impl (dispatcher fn priority)
  (bordeaux-threads:with-lock-held ((queue-lock dispatcher))
    (cl-heap:enqueue (queue dispatcher) fn priority))
  (impl:send-invoke-signal dispatcher)
  (values))

(defun %shutdown-callback-impl ()
  (let ((d (current-dispatcher)))
    (setf (shutdown-started d) t)))
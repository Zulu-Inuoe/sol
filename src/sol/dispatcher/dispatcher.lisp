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

(defvar *%dispatchers* (make-hash-table)
  "A mapping of thread-id's to `dispatcher's.
   This holds all currently existing dispatchers.")

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

(deftype dispatcher-priority ()
  `(member
    ,+priority.inactive+
    ,+priority.system-idle+
    ,+priority.application-idle+
    ,+priority.context-idle+
    ,+priority.background+
    ,+priority.input+
    ,+priority.loaded+
    ,+priority.render+
    ,+priority.databind+
    ,+priority.normal+
    ,+priority.send+))

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
  ((%queue
    :type cl-heap:priority-queue
    :initform (make-instance 'cl-heap:priority-queue :sort-fun #'>)
    :reader %queue)
   (%queue-lock
    :initform
    (bordeaux-threads:make-lock
     (format nil "DispatcherQueueLock[~A(~A);~A]"
             (lisp-implementation-type)
             (lisp-implementation-version)
             (bordeaux-threads:thread-name (bordeaux-threads:current-thread))))
    :reader %queue-lock)
   (%sync-events
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
    :accessor %sync-events)
   (%sync-lock
    :initform (bordeaux-threads:make-lock)
    :reader %sync-lock)
   (%dispatcher-frames
    :type list
    :initform nil
    :accessor %dispatcher-frames)
   (%exit-all-frames
    :type boolean
    :initform nil
    :accessor %exit-all-frames)
   (%shutdown-started
    :type boolean
    :initform nil
    :accessor %shutdown-started
    :reader has-shutdown-started)
   (%shutdown-finished
    :type boolean
    :initform nil
    :accessor %shutdown-finished
    :reader has-shutdown-finished)
   (%thread
    :type bt:thread
    :initform (bt:current-thread)
    :reader thread)
   (%e_dispatcher-inactive
    :type event
    :initform (make-instance 'event :name "dispatcher-inactive")
    :reader e_dispatcher-inactive)
   (%e_shutdown-started
    :type event
    :initform (make-instance 'event :name "shutdown-started")
    :reader e_shutdown-started)
   (%e_shutdown-finished
    :type event
    :initform (make-instance 'event :name "shutdown-finished")
    :reader e_shutdown-finished)
   (%e_unhandled-error
    :type event
    :initform (make-instance 'event :name "unhandled-error")
    :reader e_unhandled-error)))

(defmethod initialize-instance :around ((obj dispatcher) &key)
  (setf (gethash (bt:current-thread) *%dispatchers*) obj)
  (call-next-method))

(defmethod dispose ((obj dispatcher))
  (loop
     :for (thread . disp) :in (hash-table-alist *%dispatchers*)
     :when (eq obj disp)
     :do (remhash thread *%dispatchers*))
  (call-next-method))

(defmethod dispose :around ((obj dispatcher))
  (unless (or (has-shutdown-started obj)
              (has-shutdown-finished obj))
    (invoke-shutdown obj))
  (call-next-method))

(defun from-thread (thread)
  "Gets the `dispatcher' for the given thread, or NIL if no such `dispatcher' exists."
  (values (gethash thread *%dispatchers*)))

(declaim (ftype (function () dispatcher) current-dispatcher))

(defmethod check-access ((dispatcher dispatcher))
  "Returns true if the current thread has access to the given `dispatcher'."
  (eq (bt:current-thread) (thread dispatcher)))

(defgeneric %perform-invoke (obj))
(defgeneric %error-invoke (obj error))
(defgeneric %abort-invoke (obj))

(defmethod %perform-invoke ((obj function))
  (funcall obj)
  (values))

(defmethod %error-invoke ((obj function) error)
  (declare (ignore obj error))
  (values))

(defmethod %abort-invoke ((obj function))
  (declare (ignore obj))
  (values))

(defmethod %perform-invoke ((obj symbol))
  (funcall obj)
  (values))

(defmethod %error-invoke ((obj symbol) error)
  (declare (ignore obj error))
  (values))

(defmethod %abort-invoke ((obj symbol))
  (declare (ignore obj))
  (values))

(defclass dispatcher-invoke ()
  ((%fn
    :type (or null symbol function)
    :initarg :fn
    :initform nil
    :reader %fn)
   (%error-fn
    :type (or null symbol function)
    :initarg :error-fn
    :initform nil
    :reader %error-fn)
   (%abort-fn
    :type (or null symbol function)
    :initarg :abort-fn
    :initform nil
    :reader %abort-fn)))

(defmethod %perform-invoke ((obj dispatcher-invoke))
  (when-let ((fn (%fn obj)))
    (funcall fn))
  (values))

(defmethod %error-invoke ((obj dispatcher-invoke) error)
  (when-let ((error-fn (%error-fn obj)))
    (funcall error-fn error))
  (values))

(defmethod %abort-invoke ((obj dispatcher-invoke))
  (when-let ((abort-fn (%abort-fn obj)))
    (funcall abort-fn))
  (values))

(defclass dispatcher-unhandled-error-args ()
  ((%dispatcher
    :type dispatcher
    :initform (current-dispatcher)
    :reader dispatcher)
   (%args-error
    :type error
    :initform (error "Must set args error")
    :initarg :error
    :reader args-error)
   (%handled
    :type boolean
    :initform nil
    :accessor handled))
  (:documentation
   "The argument type for an unhandled dispatcher error.
    DISPATCHER is the `dispatcher' which produced the error.
    ARGS-ERROR is the `cl:error' produced.
    HANDLED allows event subscribers to specify whether the error has been handled or not."))

(defclass dispatcher-object ()
  ((%dispatcher
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
  ((%continue-frame
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
                        (not (has-shutdown-started d))
                        (continue-frame frame))
            :do
               (restart-case
                   (progn
                     (impl:wait-invoke-signal d)
                     (impl:process-queue d)
                     (unless (has-shutdown-started d)
                       (event-notify (e_dispatcher-inactive d) nil)))
                 (ignore-dispatcher-error ()
                   :report (lambda (stream)
                             (format stream "Ignore the error signalled on `dispatcher' ~A and continue." d)))))
      (pop (%dispatcher-frames d)))

    ;;Are we the last frame on the stack?
    (when (null (%dispatcher-frames d))
      ;;Then nil-out the exit-all-frames, in case that was the reason we exited
      (setf (%exit-all-frames d) nil)
      ;;If we started a shutdown
      (when (has-shutdown-started d)
        ;;The stack has unwound out, so we're done shutting down
        (setf (%shutdown-finished d) t)
        ;;Trigger shutdown-finished event
        (event-notify (e_shutdown-finished d) nil)

        ;;Abort all pending requests in the queue
        (loop
          :for invoke :=
                      (bordeaux-threads:with-lock-held ((%queue-lock d))
                        (cl-heap:dequeue (%queue d)))
          :while invoke
          :do (%abort-invoke invoke)))))
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
               &aux
                 (ret-value nil))
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
     (let* ((invoke-error nil)
            (invoked nil)
            (frame (make-instance 'dispatcher-frame))
            (hook-fn (lambda ()
                       (setf (continue-frame frame) nil
                             invoked t
                             ;;If an error is signalled from here no worries because we already set to exit the frame
                             ret-value (funcall fn))))
            (error-fn (lambda (err)
                        (setf invoke-error err)))
            (abort-fn (lambda ()
                        (setf (continue-frame frame) nil)))
            (invoke
              (make-instance 'dispatcher-invoke :fn hook-fn :error-fn error-fn :abort-fn abort-fn)))
       ;;Enqueue our message and post
       (%begin-invoke-impl dispatcher invoke priority)

       ;;Push the frame (We'll continue to process stuff until hook-fn is invoked)
       (push-frame frame)
       (when invoke-error
         (error invoke-error))
       (unless invoked
         (error "dispatcher: invoke aborted"))))
    (t
     (let (event
           lock
           condition
           (invoke-complete nil)
           (invoke-error nil)
           (invoke-aborted nil))
       ;;Wait until there's a free event by re-trying to grab it
       ;;Grab an event that we can wait on
       (tagbody
        :try-again
          (bordeaux-threads:with-lock-held ((%sync-lock dispatcher))
            ;;Are there any events left?
            (when (null (%sync-events dispatcher))
              ;;Release sync-lock and try again
              (go :try-again))
            ;;Grab it
            (setf event (pop (%sync-events dispatcher)))))

       (setf lock (car event))
       (setf condition (cdr event))

       ;;Hook-fn here will signal the event when it's called
       (let* ((hook-fn (lambda ()
                         (unwind-protect
                              (setf ret-value (funcall fn))
                           (progn
                             (setf invoke-complete t)
                             (bordeaux-threads:condition-notify condition)))))
              (error-fn (lambda (err)
                          (setf invoke-error err
                                invoke-complete t)
                          (bordeaux-threads:condition-notify condition)))
              (abort-fn (lambda ()
                          (setf invoke-aborted t
                                invoke-complete t)
                          (bordeaux-threads:condition-notify condition)))
              (invoke (make-instance 'dispatcher-invoke :fn hook-fn :error-fn error-fn :abort-fn abort-fn)))
         ;;Enqueue our message and post
         (%begin-invoke-impl dispatcher invoke priority))

       ;;Wait for the event to get signalled from our hook-fn
       (loop :until invoke-complete
          :do
          (bordeaux-threads:with-lock-held (lock)
            (bordeaux-threads:condition-wait condition lock)))
       ;;Then put it back some other people can use it
       (bordeaux-threads:with-lock-held ((%sync-lock dispatcher))
         (push event (%sync-events dispatcher)))
       (when invoke-error
         (error invoke-error))
       (when invoke-aborted
         (error "dispatcher: invoke aborted")))))
  ret-value)

(defun invoke-shutdown (dispatcher)
  "Request that the specified `dispatcher' is shutdown, and waits for it to do so."
  (invoke dispatcher #'%shutdown-callback-impl))

(defmacro do-invoke ((dispatcher &key (priority +priority.normal+)) &body body)
  `(invoke ,dispatcher (lambda () ,@body) :priority ,priority))

(defmacro do-begin-invoke ((dispatcher &key (priority +priority.normal+)) &body body)
  `(begin-invoke ,dispatcher (lambda () ,@body) :priority ,priority))

(defun impl:process-queue (dispatcher)
  "Processes one invoke item from the `dispatcher''s queue."
  (let (invoke)
    (bordeaux-threads:with-lock-held ((%queue-lock dispatcher))
      (setf invoke (cl-heap:dequeue (%queue dispatcher))))
    (when invoke
      (%process-invoke dispatcher invoke)))
  (values))

(defun %handle-dispatcher-error (d e)
  "Raises the E_UNHANDLED-ERROR event on the `dispatcher' D with the `cl:error' E.
   The event is raised with a fresh `dispatcher-unhandled-error-args', returning T if the error was handled, NIL otherwise."
  (let ((args (make-instance 'dispatcher-unhandled-error-args :error e)))
    (event-notify (e_unhandled-error d) args)
    (and (handled args)
         t)))

(defun %process-invoke (dispatcher invoke)
  "Performs the invoke with the appropriate handlers and restarts for the given `dispatcher'."
  (restart-case
      (block nil
        (handler-bind ((error (lambda (e)
                                (when (%handle-dispatcher-error dispatcher e)
                                  (return))
                                (%error-invoke invoke e))))
          (%perform-invoke invoke)))
    (ignore-dispatcher-error ()
      :report (lambda (stream)
                (format stream "Ignore the error signalled on `dispatcher' ~A and continue." dispatcher)))))

(defun %begin-invoke-impl (dispatcher invoke priority)
  (bordeaux-threads:with-lock-held ((%queue-lock dispatcher))
    (cl-heap:enqueue (%queue dispatcher) invoke priority))
  (impl:send-invoke-signal dispatcher)
  (values))

(defun %shutdown-callback-impl ()
  (let ((d (current-dispatcher)))
    (unless (has-shutdown-started d)
      (setf (%shutdown-started d) t)
      (event-notify (e_shutdown-started d) nil)
      (impl:send-shutdown-signal d)))
  (values))

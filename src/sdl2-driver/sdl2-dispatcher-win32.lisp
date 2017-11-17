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

(in-package #:sol.sdl2-driver)

(defclass sdl2-dispatcher (dispatcher:dispatcher)
  ((%shutdown-event-type
    :type integer
    :reader %shutdown-event-type)
   (%window
    :type %message-only-hwnd
    :initform (make-instance '%message-only-hwnd :wndproc (cffi:callback %sdl2-wndproc-hook))
    :reader %window)))

(defvar *%sdl2-dispatcher* nil
  "A `trivial-garbage:weak-pointer' pointing to the active sdl2-dispatcher. Used by the event filter for dispatching.")

(defmethod initialize-instance :after ((dispatcher sdl2-dispatcher) &key &allow-other-keys)
  (setf (slot-value dispatcher '%shutdown-event-type) (sdl2-ffi.functions:sdl-register-events 1))
  (when (= (%shutdown-event-type dispatcher)
           #xFFFFFFFF)
    (error "sdl2-dispatcher: error registering shutdown event type for ~A: ~A" dispatcher (sdl2-ffi.functions:sdl-get-error)))

  (sdl2-ffi.functions:sdl-set-event-filter (cffi:callback %sdl2-event-filter-callback) (cffi:null-pointer))
  (setf *%sdl2-dispatcher* (trivial-garbage:make-weak-pointer dispatcher)))

(defmethod dispose ((dispatcher sdl2-dispatcher))
  (unwind-protect
       (dispose (%window dispatcher))
    (sdl2-ffi.functions:sdl-set-event-filter (cffi:null-pointer) (cffi:null-pointer))
    (setf *%sdl2-dispatcher* nil)
    (call-next-method)))

(defconstant %+wm-invoke+ (+ win32:+wm-user+ 0)
  "The message type used for signalling to the dispatcher that there is a new work item on the queue.")

(defmethod dispatcher.impl:wait-invoke-signal ((dispatcher sdl2-dispatcher))
  (sdl2:with-sdl-event (sdl-event)
    (case (sdl2-ffi.functions:sdl-wait-event sdl-event)
      (0
       (unless (dispatcher:shutdown-started dispatcher)
         (error "sdl2-dispatcher: sdl-wait-event error: ~A" (sdl2-ffi.functions:sdl-get-error))))
      (t
       (%process-sdl-event dispatcher sdl-event))))
  nil)

(defmethod dispatcher.impl:send-invoke-signal ((dispatcher sdl2-dispatcher))
  ;;;On win32 we perform invokes as Windows Messages so we can perform invokes even
  ;;;during resize/move operations
  (win32:post-message
   (%hwnd (%window dispatcher))
   %+wm-invoke+
   (cffi:null-pointer)
   (cffi:null-pointer))
  (values))

(defmethod dispatcher.impl:send-shutdown-signal ((dispatcher sdl2-dispatcher))
  (sdl2:with-sdl-event (sdl-event)
    (setf (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :type) (%shutdown-event-type dispatcher))
    (sdl2-ffi.functions:sdl-push-event sdl-event))
  (values))

(defun %process-sdl-event (dispatcher sdl-event)
  (declare (ignore dispatcher))
  (event-notify *e_sdl2-event* sdl-event)
  (values))

(defun %do-event-filter-callback (data sdl-event)
  (declare (ignore data))
  (let* ((disp-ptr *%sdl2-dispatcher*)
         (disp (and disp-ptr (trivial-garbage:weak-pointer-value disp-ptr))))
    (unless disp
      (error "sdl2-dispatcher: no dispatcher available during callback"))
    (cond
      ((= (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :type)
          (%shutdown-event-type disp))
       ;;It's a shutdown request, so leave it on the queue
       1)
      ((dispatcher:check-access disp)
       ;;Process it now
       (%process-sdl-event disp sdl-event)
       0)
      (t ;;We can't process it now. Let SDL put it on the queue to process later
       1))))

(cffi:defcallback %sdl2-event-filter-callback :int ((data :pointer) (sdl-event :pointer))
  (%do-event-filter-callback data sdl-event))

(defun %do-wndproc-hook (hwnd msg wparam lparam)
  (case msg
    (#.%+wm-invoke+
     (let* ((disp-ptr *%sdl2-dispatcher*)
            (disp (and disp-ptr (trivial-garbage:weak-pointer-value disp-ptr))))
       (unless disp
         (error "sdl2-dispatcher: no dispatcher available during wndproc"))
       (dispatcher.impl:process-queue disp))
     0)
    (otherwise
     (win32:def-window-proc hwnd msg wparam lparam))))

(cffi:defcallback (%sdl2-wndproc-hook :convention :stdcall) win32:lresult
    ((hwnd win32:hwnd) (msg win32:uint) (wparam win32:wparam) (lparam win32:lparam))
  (%do-wndproc-hook hwnd msg wparam lparam))

(defun %make-guid ()
  "Create a randomly-generated 128-bit GUID in the form of a {} wrapped, 32-digit, hyphen-separated, hexadecimal string.
  Example: {A3C78807-EA6D-A4AE-1CCA-797A6BF88C31}"
  (let ((value (random (ash 1 128))))
    (format nil "{~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X}"
            (ldb (byte 32 96) value)
            (ldb (byte 16 80) value)
            (ldb (byte 16 64) value)
            (ldb (byte 16 48) value)
            (ldb (byte 48 0) value))))

(defun %memset (ptr value num)
  "A wicked slow memset"
  (dotimes (i num)
    (setf (cffi:mem-aref ptr :int8 i) value)))

(defclass %message-only-hwnd (finalizable)
  ((%hwnd
    :reader %hwnd)
   (%class-atom
    :reader %class-atom)
   (%wndproc
    :type cffi:foreign-pointer
    :initarg :wndproc
    :initform (cffi:foreign-symbol-pointer "DefWindowProcW")
    :reader %wndproc)))

(defmethod initialize-instance :after ((this %message-only-hwnd) &key &allow-other-keys)
  (let* ((wndclass-name (format nil "MessageOnlyHwnd[~A(~A);~A;~A]"
                                (lisp-implementation-type)
                                (lisp-implementation-version)
                                (bordeaux-threads:thread-name (bordeaux-threads:current-thread))
                                (%make-guid))))
    (cffi:with-foreign-object (class '(:struct win32:wndclassex))
      (%memset class 0 (cffi:foreign-type-size '(:struct win32:wndclassex)))
      (cffi:with-foreign-slots ((win32:cbsize win32:wndproc win32:instance win32:wndclass-name) class (:struct win32:wndclassex))
        (setf win32:cbsize (cffi:foreign-type-size '(:struct win32:wndclassex))
              win32:wndproc (%wndproc this)
              win32:instance (win32:get-module-handle (cffi:null-pointer))
              win32:wndclass-name wndclass-name))
      (setf (slot-value this '%class-atom) (win32:register-class-ex class)
            (slot-value this '%hwnd)
            (win32:create-window-ex
             0
             wndclass-name
             wndclass-name
             0
             0 0 0 0
             (cffi:make-pointer win32:+hwnd-message+)
             (cffi:null-pointer)
             (cffi:null-pointer)
             (cffi:null-pointer))))))

(define-finalizer %message-only-hwnd (%hwnd %class-atom)
  (win32:destroy-window %hwnd)
  (win32:unregister-class (cffi:make-pointer %class-atom) (cffi:null-pointer)))
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

(in-package #:sol.drivers.sdl2)

(defclass sdl2-dispatcher-impl (finalizable)
  ((dispatcher
    :type dispatcher:dispatcher
    :initarg :dispatcher
    :initform (error "sdl2-dispatcher-impl: must supply dispatcher")
    :reader dispatcher)
   (e_sdl-event
    :type event
    :initform (make-instance 'event :name "sdl-event")
    :reader e_sdl-event)
   (%invoke-event-type
    :type integer
    :reader %invoke-event-type)))

(defvar *%sdl2-dispatcher-impl* nil
  "A `trivial-garbage:weak-pointer' pointing to the active sdl2-dispatcher. Used by the event filter for dispatching.")

(defmethod initialize-instance :after ((impl sdl2-dispatcher-impl) &key &allow-other-keys)
  (format t "Initializing dispatcher ~A~%" impl)
  (sdl2-ffi.functions:sdl-set-event-filter (cffi:callback %sdl2-event-filter-callback) (cffi:null-pointer))

  (setf (slot-value impl '%invoke-event-type) (sdl2-ffi.functions:sdl-register-events 1))
  (when (= (%invoke-event-type impl)
           #xFFFFFFFF)
    (error "sdl2-dispatcher-impl: error registering invoke event type for ~A: ~A" impl (sdl2-ffi.functions:sdl-get-error)))

  (setf *%sdl2-dispatcher-impl* (trivial-garbage:make-weak-pointer impl)))

(define-finalizer sdl2-dispatcher-impl ()
  (sdl2-ffi.functions:sdl-set-event-filter (cffi:null-pointer) (cffi:null-pointer))
  (setf *%sdl2-dispatcher-impl* nil))

(defmethod dispatcher.impl:send-invoke-signal ((impl sdl2-dispatcher-impl))
  (sdl2:with-sdl-event (sdl-event)
    (setf (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :type) (%invoke-event-type impl))
    (sdl2-ffi.functions:sdl-push-event sdl-event))
  (values))

(defmethod dispatcher.impl:wait-invoke-signal ((impl sdl2-dispatcher-impl))
  (sdl2:with-sdl-event (sdl-event)
    (when (and (zerop (sdl2-ffi.functions:sdl-wait-event sdl-event))
               (not (dispatcher:shutdown-started (dispatcher impl))))
      (error "sdl2-dispatcher-impl: sdl-wait-event error: ~A" (sdl2-ffi.functions:sdl-get-error)))
    nil))

(cffi:defcallback %sdl2-event-filter-callback :int ((data :pointer) (sdl-event :pointer))
  (declare (ignore data))
  (let* ((impl-ptr *%sdl2-dispatcher-impl*)
         (impl (and impl-ptr (trivial-garbage:weak-pointer-value impl-ptr))))
    (unless impl
      (error "sdl2-dispatcher-impl: no impl available during callback"))
    (if (= (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :type)
           (%invoke-event-type impl))
        (dispatcher.impl:process-queue (dispatcher impl))
        (event-notify (e_sdl-event impl) sdl-event))
    1))
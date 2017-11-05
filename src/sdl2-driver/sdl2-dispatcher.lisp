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
  ((%invoke-event-type
    :type integer
    :reader %invoke-event-type)
   (%shutdown-event-type
    :type integer
    :reader %shutdown-event-type)))

(defmethod initialize-instance :after ((dispatcher sdl2-dispatcher) &key &allow-other-keys)
  (setf (slot-value dispatcher '%invoke-event-type) (sdl2-ffi.functions:sdl-register-events 2))
  (when (= (%invoke-event-type dispatcher)
           #xFFFFFFFF)
    (error "sdl2-dispatcher: error registering invoke event type for ~A: ~A" dispatcher (sdl2-ffi.functions:sdl-get-error)))
  (setf (slot-value dispatcher '%shutdown-event-type) (1+ (%invoke-event-type dispatcher))))

(defmethod dispatcher.impl:wait-invoke-signal ((dispatcher sdl2-dispatcher))
  (sdl2:with-sdl-event (sdl-event)
    (case (sdl2-ffi.functions:sdl-wait-event sdl-event)
      (0
       (error "sdl2-dispatcher: sdl-wait-event error: ~A" (sdl2-ffi.functions:sdl-get-error)))
      (t
       (%process-sdl-event dispatcher sdl-event))))
  nil)

(defmethod dispatcher.impl:send-invoke-signal ((dispatcher sdl2-dispatcher))
  (sdl2:with-sdl-event (sdl-event)
    (setf (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :type) (%invoke-event-type dispatcher))
    (sdl2-ffi.functions:sdl-push-event sdl-event))
  (values))

(defmethod dispatcher.impl:send-shutdown-signal ((dispatcher sdl2-dispatcher))
  (sdl2:with-sdl-event (sdl-event)
    (setf (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :type) (%shutdown-event-type dispatcher))
    (sdl2-ffi.functions:sdl-push-event sdl-event))
  (values))

(defun %process-sdl-event (dispatcher sdl-event)
  (switch ((plus-c:c-ref sdl-event sdl2-ffi:sdl-event :type))
    ((%invoke-event-type dispatcher)
     ;;If it's an invoke request, process it
     (dispatcher.impl:process-queue dispatcher))
    ((%shutdown-event-type dispatcher)
     ;;If it's a shutdown request, just ignore it.
     ;;It was used to pump the queue
     (values))
    (t
     ;;Otherwise, pass on the sdl event
     (event-notify *e_sdl2-event* sdl-event)))
  (values))
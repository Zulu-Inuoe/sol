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

#+fake-fake
(defvar %*sdl2-dispatcher* nil)

(deftype wait-behaviour ()
  `(member :wait :poll))

(defclass sdl2-dispatcher (dispatcher:dispatcher disposeable)
  ((%wait-behaviour
    :type wait-behaviour
    :initform :wait
    :accessor wait-behaviour)
   #+fake-fake
   (%window
    :type %message-only-hwnd
    :initform (make-instance '%message-only-hwnd :wndproc (cffi:callback %sdl2-wndproc-hook))
    :reader %window)))

(defmethod initialize-instance :after ((dispatcher sdl2-dispatcher) &key &allow-other-keys)
  #+fake-fake
  (setf %*sdl2-dispatcher* dispatcher)
  #+fake-fake
  (raw-bindings-sdl2:sdl-set-event-filter (cffi:callback %sdl2-event-filter-callback) (cffi:null-pointer)))

(defmethod dispose ((dispatcher sdl2-dispatcher))
  #+fake-fake
  (raw-bindings-sdl2:sdl-set-event-filter (cffi:null-pointer) (cffi:null-pointer))
  #+fake-fake
  (setf %*sdl2-dispatcher* dispatcher)
  #+fake-fake
  (dispose (%window dispatcher)))

(defmethod dispatcher.impl:wait-invoke-signal ((dispatcher sdl2-dispatcher))
  (cffi:with-foreign-object (sdl-event 'sdl2:sdl-event)
    (ecase (wait-behaviour dispatcher)
      (:wait
       (case (sdl2:sdl-wait-event sdl-event)
         (1
          (%process-sdl-event dispatcher sdl-event))
         (t
          (unless (dispatcher:has-shutdown-started dispatcher)
            (error "sdl2-dispatcher: sdl-wait-event error: ~A" (sdl2:sdl-get-error))))))
      (:poll
       (case (sdl2:sdl-poll-event sdl-event)
         (1
          (%process-sdl-event dispatcher sdl-event))
         (t
          nil)))))
  nil)

#+fake-fake
(defconstant %+wm-invoke+ (+ win32:+wm-user+ 0)
  "The message type used for signalling to the dispatcher that there is a new work item on the queue.
On Windows, the modal window handling mechanism causes SDL2 to stop responding to SDL events.
SDL's WndProc will queue up messages as they are pushed, until the modal loop is exited and only then
shall SDL_WaitMessage and friends return.
Instead, a custom HWND is created and used to signal invoke requests.
By virtue of PostMessage and WndProc, the correct thread shall be in execution during these calls.")

#+fake-fake
(defconstant %+wm-sdl2-event-push+ (+ win32:+wm-user+ 1)
  "The message type used for signalling to the dispatcher that there is a sdl2-event available in the SDL queue.")

(defmethod dispatcher.impl:send-invoke-signal ((dispatcher sdl2-dispatcher))
  ;;TODO temp hack
  (when *sdl2-invoke-event-type*
    (cffi:with-foreign-object (sdl-event 'sdl2:sdl-event)
      (setf (cffi:foreign-slot-value sdl-event 'sdl2:sdl-event 'sdl2:type) *sdl2-invoke-event-type*)
      (sdl2:sdl-push-event sdl-event)))
  (values))

(defmethod dispatcher.impl:send-shutdown-signal ((dispatcher sdl2-dispatcher))
  ;;TODO temp hack
  (when *sdl2-shutdown-event-type*
    (cffi:with-foreign-object (sdl-event 'sdl2:sdl-event)
      (setf (cffi:foreign-slot-value sdl-event 'sdl2:sdl-event 'sdl2:type) *sdl2-shutdown-event-type*)
      (sdl2:sdl-push-event sdl-event)))
  (values))

(defun %event-window-id (sdl-event
                         &aux
                           (sdl-event-type (cffi:foreign-slot-value sdl-event 'sdl2:sdl-event 'sdl2:type)))
  "Get the window ID of the given event, or nil if the event type is not aimed at a window."
  (case sdl-event-type
    ((#.sdl2:+sdl-mousebuttondown+ #.sdl2:+sdl-mousebuttonup+)
     (cffi:foreign-slot-value sdl-event 'sdl2:sdl-mouse-button-event 'sdl2:window-id))
    (#.sdl2:+sdl-mousemotion+
     (cffi:foreign-slot-value sdl-event 'sdl2:sdl-mouse-motion-event 'sdl2:window-id))
    (#.sdl2:+sdl-mousewheel+
     (cffi:foreign-slot-value sdl-event 'sdl2:sdl-mouse-wheel-event 'sdl2:window-id))
    (#.sdl2:+sdl-window-event+
     (cffi:foreign-slot-value sdl-event 'sdl2:sdl-window-event 'sdl2:window-id))
    ((#.sdl2:+sdl-keydown+ #.sdl2:+sdl-keyup+)
     (cffi:foreign-slot-value sdl-event 'sdl2:sdl-keyboard-event 'sdl2:window-id))
    (#.sdl2:+sdl-textinput+
     (cffi:foreign-slot-value sdl-event 'sdl2:sdl-text-input-event 'sdl2:window-id))
    (#.sdl2:+sdl-textediting+
     (cffi:foreign-slot-value sdl-event 'sdl2:sdl-text-editing-event 'sdl2:window-id))
    (t nil)))

(defun %get-window-impl-from-id (window-id)
  (when-let ((window-impl-ptr (gethash window-id %*sdl2-windows*)))
    (tg:weak-pointer-value window-impl-ptr)))

(defun %get-event-owner-window-impl (sdl-event)
  (when-let ((window-id (%event-window-id sdl-event)))
    (%get-window-impl-from-id window-id)))

(defun %route-sdl-event-to-window-impl (sdl-event)
  ;;Route it to the right window
  (when-let ((window-impl (%get-event-owner-window-impl sdl-event)))
    (%sdl2-window-impl.sdl2-event window-impl sdl-event)))

(defun %process-sdl-event (dispatcher sdl-event
                           &aux
                             (event-type (cffi:foreign-slot-value sdl-event 'sdl2:sdl-event 'sdl2:type)))
  (switch (event-type)
    (*sdl2-invoke-event-type*
     ;;If it's an invoke request, process it
     (dispatcher.impl:process-queue dispatcher))
    (*sdl2-shutdown-event-type*
     ;;If it's a shutdown request, just ignore it.
     ;;It was used to pump the queue
     (values))
    (t
     ;;Route it to the owner window
     (%route-sdl-event-to-window-impl sdl-event)
     ;;Notify global event handlers
     (event-notify *e_sdl2-event* sdl-event)))
  (values))

#+fake-fake
(defun %do-event-filter-callback (data sdl-event)
  (declare (ignore data sdl-event))
  (unless %*sdl2-dispatcher*
    (error "sdl2-dispatcher: no dispatcher available during event filter"))
  (win32:post-message (%hwnd (%window %*sdl2-dispatcher*)) %+wm-sdl2-event-push+ 0 0)
  1)

#+fake-fake
(cffi:defcallback %sdl2-event-filter-callback :int ((data :pointer) (sdl-event :pointer))
  (%do-event-filter-callback data sdl-event))

#+fake-fake
(defun %do-wndproc-hook (hwnd msg wparam lparam
                         &aux
                           (dispatcher (dispatcher:from-thread (bt:current-thread))))
  (unless dispatcher
    (error "sdl2-dispatcher: no dispatcher available during wndproc"))
  (case msg
    (#.%+wm-invoke+
     (dispatcher.impl:process-queue dispatcher))
    (#.%+wm-sdl2-event-push+
     ;; Process events
     (cffi:with-foreign-object (sdl-event 'sdl2:sdl-event)
       (loop
       :while (plusp (sdl2:sdl-peep-events sdl-event 1 sdl2:+sdl-getevent+ sdl2:+sdl-firstevent+ sdl2:+sdl-lastevent+))
       :do (%process-sdl-event dispatcher sdl-event)
       :until (/= (cffi:foreign-slot-value sdl-event 'sdl2:sdl-event 'sdl2:type)
                  ;;Eat up all the syswm events until we get to a 'real' one
                  sdl2:+sdl-syswmevent+)))))

  (event-notify (dispatcher:e_dispatcher-inactive dispatcher) nil)

  (win32:def-window-proc hwnd msg wparam lparam))

#+fake-fake
(cffi:defcallback (%sdl2-wndproc-hook :convention :stdcall) win32:lresult
    ((hwnd win32:hwnd) (msg win32:uint) (wparam win32:wparam) (lparam win32:lparam))
  (%do-wndproc-hook hwnd msg wparam lparam))

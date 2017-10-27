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

(defclass sdl2-gpu-driver (finalizable)
  ((window-callback
    :initarg :window-callback
    :initform nil
    :reader window-callback)
   (dispatcher
    :type dispatcher:dispatcher
    :reader dispatcher)
   (input-manager
    :type sdl2-input-manager
    :reader input-manager)
   (windows
    :type cons
    :initform (cons nil nil)
    :reader windows)))

(defmethod initialize-instance :after ((driver sdl2-gpu-driver) &key &allow-other-keys)
  (sdl2-ffi.functions:sdl-init (autowrap:mask-apply 'sdl2::sdl-init-flags (list :everything)))
  (sdl2-ffi.functions:sdl-set-hint "SDL_MOUSE_FOCUS_CLICKTHROUGH" "1")
  (sdl2-image:init (list :png :jpg :tif))
  (sdl2-ttf:init)
  ;;GPU init is done on each window individually

  (setf (dispatcher.impl:current-dispatcher-impl-fn)
        (lambda (d)
          (make-instance 'sdl2-dispatcher-impl :dispatcher d)))
  (setf (ui.impl:current-window-impl-fn)
        (lambda (ui-window
            &key
              title
              x y
              width height
              state border-style
              fullscreen visible)
          (push (trivial-garbage:make-weak-pointer ui-window) (car (windows driver)))
          (when (window-callback driver)
            (funcall (window-callback driver) ui-window))
          (make-instance
           'sdl2-gpu-window-impl
           :ui-window ui-window
           :title title
           :x x :y y
           :width width :height height
           :state state :border-style border-style
           :fullscreen fullscreen :visible visible)))

  (setf (slot-value driver 'dispatcher) (dispatcher:current-dispatcher))
  (setf (slot-value driver 'input-manager)
        (make-instance 'sdl2-input-manager
                       :e_sdl-event (e_sdl-event (dispatcher.impl:impl (dispatcher driver)))))
  (media::media-init))

(define-finalizer sdl2-gpu-driver (dispatcher input-manager windows)
  (loop
     :for wptr :in (car windows)
     :for w := (trivial-garbage:weak-pointer-value wptr)
     :if w
     :do (dispose w))
  (setf (car windows) nil)

  (media::media-uninit)

  (dispose input-manager)
  (dispose dispatcher)

  (when *%gpu-already-init*
    (setf *%gpu-already-init* nil)
    (sdl2-ffi.functions:gpu-quit))

  (sdl2-ttf:quit)
  (sdl2-image:quit)
  (sdl2-ffi.functions:sdl-quit))

(defmethod driver-dispatcher ((driver sdl2-gpu-driver))
  (dispatcher driver))

(define-driver sdl2-gpu-driver)
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

(defclass sdl2-driver (disposeable)
  ((dispatcher
    :type dispatcher:dispatcher
    :reader dispatcher)
   (font-context
    :type sdl2-font-context
    :reader font-context)
   (image-context
    :type sdl2-image-context
    :reader image-context)))

(defmethod initialize-instance :after ((driver sdl2-driver) &key &allow-other-keys)
  #+sbcl
  (sb-int:with-float-traps-masked (:underflow
                                   :overflow
                                   :inexact
                                   :invalid
                                   :divide-by-zero)
    (sdl2-ffi.functions:sdl-init (autowrap:mask-apply 'sdl2::sdl-init-flags (list :everything))))
  #-sbcl
  (sdl2-ffi.functions:sdl-init (autowrap:mask-apply 'sdl2::sdl-init-flags (list :everything)))
  (sdl2-ffi.functions:sdl-set-hint "SDL_MOUSE_FOCUS_CLICKTHROUGH" "1")
  (sdl2-image:init (list :png :jpg :tif))
  (sdl2-ttf:init)

  (let (success)
    (unwind-protect
         (progn
           (dispose-on-error
             (setf (slot-value driver 'dispatcher) (make-instance 'sdl2-dispatcher))
             (setf (slot-value driver 'font-context) (make-instance 'sdl2-font-context))
             (setf (slot-value driver 'image-context) (make-instance 'sdl2-image-context)))
           (setf success t))
      (unless success
        (sdl2-ttf:quit)
        (sdl2-image:quit)
        (sdl2-ffi.functions:sdl-quit)))))

(defmethod dispose ((driver sdl2-driver))
  (unwind-protect
       (ensure-dispose
         (image-context driver)
         (font-context driver)
         (dispatcher driver))

    (sdl2-ttf:quit)
    (sdl2-image:quit)
    (sdl2-ffi.functions:sdl-quit)
    (call-next-method)))

(defmethod driver-dispatcher ((driver sdl2-driver))
  (dispatcher driver))

(defmethod driver-window-impl ((driver sdl2-driver))
  'sdl2-window-impl)

(defmethod driver-font-impl ((driver sdl2-driver))
  'sdl2-font-impl)

(defmethod driver-text-impl ((driver sdl2-driver))
  'sdl2-text-impl)

(defmethod driver-image-impl ((driver sdl2-driver))
  'sdl2-image-impl)

(define-driver sdl2-driver)
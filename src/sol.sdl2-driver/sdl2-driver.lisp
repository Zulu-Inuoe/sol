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
  ((dispatcher-impl
    :type (or symbol class)
    :initform 'sdl2-dispatcher
    :initarg :dispatcher-impl
    :reader dispatcher-impl)
   (window-impl
    :type (or symbol class)
    :initform 'sdl2-window-impl
    :initarg :window-impl
    :reader window-impl)
   (dispatcher
    :type dispatcher:dispatcher
    :reader dispatcher)
   (font-context
    :type sdl2-font-context
    :reader font-context)
   (image-context
    :type sdl2-image-context
    :reader image-context)
   (input-manager
    :type sdl2-input-manager
    :reader input-manager)))

(defmethod initialize-instance :after ((driver sdl2-driver) &key &allow-other-keys)
  #+sbcl
  (sb-int:with-float-traps-masked (:underflow
                                   :overflow
                                   :inexact
                                   :invalid
                                   :divide-by-zero)
    (sdl2:sdl-init sdl2:+sdl-init-everything+))
  #-sbcl
  (sdl2:sdl-init (autowrap:mask-apply 'sdl2::sdl-init-flags (list :everything)))
  (sdl2:sdl-set-hint sdl2:+sdl-hint-mouse-focus-clickthrough+ "1")
  (sdl2-image:img-init (logior sdl2-image:+img-init-jpg+
                               sdl2-image:+img-init-png+
                               sdl2-image:+img-init-tif+))
  (sdl2-ttf:ttf-init)

  ;;TODO turning on sdl-syswmevent made dispatching exceedingly slow,
  ;; but it's the only way I could find to get around the 'update while resize/move'
  ;; freeze on windows
  #+fake-fake
  (sdl2:sdl-event-state sdl2:+sdl-syswmevent+ sdl2:+sdl-enable+)

  (let (success)
    (unwind-protect
         (progn
           (setf *sdl2-invoke-event-type* (sdl2:sdl-register-events 2))
           (when (= *sdl2-invoke-event-type* #xFFFFFFFF)
             (error "sdl2-driver: error registering invoke event type for ~A: ~A" driver (sdl2:sdl-get-error)))
           (setf *sdl2-shutdown-event-type* (1+ *sdl2-invoke-event-type*))
           (dispose-on-error
             (setf (slot-value driver 'dispatcher) (make-instance (dispatcher-impl driver)))
             (setf (slot-value driver 'font-context) (make-instance 'sdl2-font-context))
             (setf (slot-value driver 'image-context) (make-instance 'sdl2-image-context))
             (setf (slot-value driver 'input-manager) (make-instance 'sdl2-input-manager)))
           (setf success t))
      (unless success
        (setf *sdl2-shutdown-event-type* nil
              *sdl2-invoke-event-type* nil)
        (sdl2-ttf:ttf-quit)
        (sdl2-image:img-quit)
        (sdl2:sdl-quit)))))

(defmethod dispose ((driver sdl2-driver))
  (unwind-protect
       (ensure-dispose
				 (input-manager driver)
         (image-context driver)
         (font-context driver)
         (dispatcher driver))

    (setf *sdl2-shutdown-event-type* nil
          *sdl2-invoke-event-type* nil)

    (sdl2-ttf:ttf-quit)
    (sdl2-image:img-quit)
    (sdl2:sdl-quit)
    (call-next-method)))

(defmethod driver-layout-manager ((driver sdl2-driver))
  driver)

(defmethod driver-dispatcher ((driver sdl2-driver))
  (dispatcher driver))

(defmethod driver-window-impl ((driver sdl2-driver))
  (window-impl driver))

(defmethod driver-font-impl ((driver sdl2-driver))
  'sdl2-font-impl)

(defmethod driver-text-impl ((driver sdl2-driver))
  'sdl2-text-impl)

(defmethod driver-image-impl ((driver sdl2-driver))
  'sdl2-image-impl)

(defmethod request-arrange ((layout-manager sdl2-driver) component)
  ;;Look for the window that owns this component
  (when (typep component 'ui:window)
    ;;Try to find the associated sdl2-window
    (maphash-values
     (lambda (w-ptr)
       (when-let* ((w (tg:weak-pointer-value w-ptr))
                   (is-owner (eq (ui-window w) component)))
         (dispatcher:begin-invoke
          (dispatcher:dispatcher component)
          (lambda ()
            (unless (closed w)
              (sdl-draw w)))
          :priority
          dispatcher:+priority.render+)))
     %*sdl2-windows*)))

(define-driver sdl2-driver)

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

(defclass sdl2-gpu-window-impl (sdl2-window-impl)
  ((gpu-target
    :type cffi:foreign-pointer
    :reader gpu-target)))

(defvar *%gpu-already-init* nil)
(defmethod sdl-create-renderer ((impl sdl2-gpu-window-impl) sdl-window
                                &aux
                                  (window-id (sdl2:sdl-get-window-id sdl-window)))
  (let ((gpu-target
         (cond
           (*%gpu-already-init*
            (sdl2-gpu:gpu-create-target-from-window window-id))
           (t
            (sdl2-gpu:gpu-set-init-window window-id)
            (prog1 (sdl2-gpu:gpu-init 0 0 0)
              (setf *%gpu-already-init* t))))))
    (setf (slot-value impl 'gpu-target) gpu-target)
    (make-instance 'sdl2-gpu-renderer :gpu-target gpu-target)))

(defmethod sdl-window-event ((impl sdl2-gpu-window-impl) event-type data1 data2)
  (case event-type
    ((:size-changed :resized)
     (sdl2-gpu:gpu-make-current (gpu-target impl) (sdl2:sdl-get-window-id (sdl-window impl)))
     (sdl2-gpu:gpu-set-window-resolution data1 data2)))

  (call-next-method))

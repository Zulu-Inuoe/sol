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

(defclass sdl2-text-impl ()
  ((media-text
    :type media:text
    :initarg :text
    :initform (error "sdl2-text-impl: must supply text")
    :reader media-text)
   (sdl-surface
    :type (or null sdl2-ffi:sdl-surface)
    :reader sdl-surface)))

(defmethod initialize-instance :after ((impl sdl2-text-impl) &key &allow-other-keys)
  (setf (slot-value impl 'sdl-surface) (%render-text (media-text impl))))

(defmethod text-width ((impl sdl2-text-impl)
                       &aux
                         (surface (sdl-surface impl)))
  (if surface
      (plus-c:c-ref surface sdl2-ffi:sdl-surface :w)
      0))

(defmethod text-height ((impl sdl2-text-impl)
                        &aux
                          (surface (sdl-surface impl)))
  (if surface
      (plus-c:c-ref surface sdl2-ffi:sdl-surface :h)
      0))

(defmethod text-set-dirty ((impl sdl2-text-impl))
  (when-let ((old-surface (sdl-surface impl)))
    (sdl2:free-surface old-surface)
    (setf (slot-value impl 'sdl-surface) nil))

  (setf (slot-value impl 'sdl-surface) (%render-text (media-text impl))))

(defun %render-text (text
                     &aux
                       (text-string (media:text-string text))
                       (font (media:text-font text))
                       (color (media:text-color text)))
  (when (zerop (length text-string))
    (return-from %render-text))

  (sdl2-ttf:render-utf8-blended
   (ttf-font (media:font-impl font))
   text-string
   (media:r color)
   (media:g color)
   (media:b color)
   (media:a color)))
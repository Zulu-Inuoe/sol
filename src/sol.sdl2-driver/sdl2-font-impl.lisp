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

(defclass sdl2-font-impl ()
  ((media-font
    :type media:font
    :initarg :font
    :initform (error "sdl2-font-impl: must supply font")
    :reader %media-font)
   (ttf-font
    :type sdl2-ttf:ttf-font
    :reader ttf-font)))

(defmethod initialize-instance :after ((impl sdl2-font-impl) &key &allow-other-keys)
  (setf (slot-value impl 'ttf-font) (%load-font (%media-font impl))))

(defmethod font-height ((impl sdl2-font-impl))
  (sdl2-ttf:ttf-font-height (ttf-font impl)))

(defmethod font-size-text ((impl sdl2-font-impl) text)
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2-ttf:ttf-size-utf8 (ttf-font impl) text w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

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

(defvar *%image-context* nil)

(defclass sdl2-image-context ()
  ((%loaded-surfaces
    :documentation "(path . packed-color-key) <=> (surface . refcont)"
    :initform (make-hash-table :test 'equalp)
    :reader %loaded-surfaces)))

(defmethod initialize-instance :after ((context sdl2-image-context) &key &allow-other-keys)
  (unless (null *%image-context*)
    (error "sdl2-image-context: context already exists"))
  (setf *%image-context* context))

(defmethod dispose ((context sdl2-image-context))
  (unless (eq *%image-context* context)
    (error "sdl2-image-context: multiple contexts"))
  (setf *%image-context* nil)
  (maphash
   (lambda (k v)
     (format t "image-context: destroying surface ~A ~A~%" k (cdr v))
     (sdl2:free-surface (car v)))
   (%loaded-surfaces context))
  (clrhash (%loaded-surfaces context)))

(defun %cache-image-surface (path color-key
                             &aux
                               (context *%image-context*)
                               (key (cons path (media:pack-color color-key)))
                               (map (%loaded-surfaces context)))
  (let ((surface (gethash key map)))
    (unless surface
      (setf surface (cons (sdl2-image:load-image path) 0))
      (unless (media:color= color-key media.colors:*transparent*)
        (let ((mapped-color
               (sdl2-ffi.functions:sdl-map-rgba
                (plus-c:c-ref (car surface) sdl2-ffi:sdl-surface :format)
                (media:r color-key)
                (media:g color-key)
                (media:b color-key)
                (media:a color-key))))
          (sdl2-ffi.functions:sdl-set-color-key (car surface) sdl2-ffi:+true+ mapped-color)))
      (setf (gethash key map) surface))
    (incf (cdr surface))
    (car surface)))

(defun %uncache-image-surface (path color-key
                               &aux
                                 (context *%image-context*)
                                 (key (cons path (media:pack-color color-key)))
                                 (map (%loaded-surfaces context)))
  (let ((surface (gethash key map)))
    (unless surface
      (error "sdl2-image-context: attempting to uncache unknown surface '~A'" path))
    (decf (cdr surface))
    (when (zerop (cdr surface))
      (format t "sdl2-image-context: destroying surface ~A~%" path)
      (sdl2:free-surface (car surface))
      (remhash key map)))
  (values))
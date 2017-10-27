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

(in-package #:sol.media)

(defclass image ()
  ((path
    :type pathname
    :reader image-path)
   (color-key
    :type color
    :initarg :color-key
    :initform *transparent*
    :reader image-color-key)
   (%image-sdl-surface
    :type sdl2-ffi:sdl-surface)
   (%src-x
    :type integer)
   (%src-y
    :type integer)
   (%des-w
    :type (or null integer))
   (%des-h
    :type (or null integer))
   (%src-w
    :type integer)
   (%src-h
    :type integer)))

(defmethod print-object ((i image) stream)
  (print-unreadable-object (i stream :type t)
    (format stream  "~S" (image-path i))))

(defmethod initialize-instance :after ((image image)
                                       &key
                                         (path (error "image: must supply path"))
                                         x y width height
                                         &allow-other-keys)
  (etypecase path
    ((or string pathname)
     (setf path (pathname path))))
  ;;TODO workaround for https://bugs.launchpad.net/sbcl/+bug/1712944
  (format nil "~A" path)
  (setf (slot-value image 'path) path)

  (let* ((x (truncate (or x 0)))
         (y (truncate (or y 0)))
         (width (and width (truncate width)))
         (height (and height (truncate height))))
    (setf (slot-value image '%src-x) x)
    (setf (slot-value image '%src-y) y)
    (setf (slot-value image '%des-w) width)
    (when width
      (setf (slot-value image '%src-w) width))
    (setf (slot-value image '%des-h) height)
    (when height
      (setf (slot-value image '%src-h) height))))

(defmethod dispose ((image image))
  (when (slot-boundp image '%image-sdl-surface)
    (trivial-garbage:cancel-finalization image)
    (funcall (%image-make-dispose-fn image))
    (slot-makunbound image '%image-sdl-surface)
    (unless (slot-value image '%des-w)
      (slot-makunbound image '%src-w))
    (unless (slot-value image '%des-h)
      (slot-makunbound image '%src-h)))
  (values))

(defun image-width (image)
  (unless (slot-boundp image '%src-w)
    (%image-ensure-loaded image))
  (slot-value image '%src-w))

(defun image-height (image)
  (unless (slot-boundp image '%src-h)
    (%image-ensure-loaded image))
  (slot-value image '%src-h))

(defun %image-make-dispose-fn (image)
  (let ((path (image-path image))
        (color-key (image-color-key image)))
    (lambda ()
      (%uncache-image-surface path color-key))))

(defun %image-ensure-loaded (image &aux (path (image-path image)) (color-key (image-color-key image)))
  (unless (slot-boundp image '%image-sdl-surface)
    (let ((surface (%cache-image-surface path color-key)))
      (setf (slot-value image '%image-sdl-surface) surface)
      (let* ((x (slot-value image '%src-x))
             (y (slot-value image '%src-y))
             (width (or (slot-value image '%des-w) (sdl2:surface-width surface)))
             (height (or (slot-value image '%des-h) (sdl2:surface-height surface))))
        (setf (slot-value image '%src-x) x)
        (setf (slot-value image '%src-y) y)
        (setf (slot-value image '%src-w) width)
        (setf (slot-value image '%src-h) height)))
    (trivial-garbage:finalize image (%image-make-dispose-fn image))))

(defun %image-sdl-surface (image)
  (%image-ensure-loaded image)
  (slot-value image '%image-sdl-surface))

(defclass %image-context ()
  ((%loaded-surfaces
    :documentation "(path . packed-color-key) <=> (surface . refcont)"
    :initform (make-hash-table :test 'equalp)
    :reader %loaded-surfaces)))

(defvar *%image-context* nil)

(defun %image-context ()
  (media-init)
  *%image-context*)

(defun %image-context-destroy (context)
  (maphash
   (lambda (k v)
     (format t "image-context: destroying surface ~A ~A~%" k (cdr v))
     (sdl2:free-surface (car v)))
   (%loaded-surfaces context))
  (clrhash (%loaded-surfaces context)))

(defun %cache-image-surface (path color-key
                             &aux
                               (context (%image-context))
                               (key (cons path (pack-color color-key)))
                               (map (%loaded-surfaces context)))
  (let ((surface (gethash key map)))
    (unless surface
      (setf surface (cons (sdl2-image:load-image path) 0))
      (unless (color= color-key *transparent*)
        (let ((mapped-color
               (sdl2-ffi.functions:sdl-map-rgba
                (plus-c:c-ref (car surface) sdl2-ffi:sdl-surface :format)
                (r color-key)
                (g color-key)
                (b color-key)
                (a color-key))))
          (sdl2-ffi.functions:sdl-set-color-key (car surface) sdl2-ffi:+true+ mapped-color)))
      (setf (gethash key map) surface))
    (incf (cdr surface))
    (car surface)))

(defun %uncache-image-surface (path color-key
                               &aux
                                 (context (%image-context))
                                 (key (cons path (pack-color color-key)))
                                 (map (%loaded-surfaces context)))
  (let ((surface (gethash key map)))
    (unless surface
      (error "image-context: attempting to uncache unknown surface '~A'" path))
    (decf (cdr surface))
    (when (zerop (cdr surface))
      (format t "image-context: destroying surface ~A~%" path)
      (sdl2:free-surface (car surface))
      (remhash key map)))
  (values))
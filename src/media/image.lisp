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
    :initform colors:*transparent*
    :reader image-color-key)
   (impl
    :reader image-impl)))

(defmethod initialize-instance :after ((image image)
                                       &key
                                         (path (error "image: must supply path"))
                                         &allow-other-keys)
  (etypecase path
    ((or string pathname)
     (setf path (pathname path))))
  (setf (slot-value image 'path) path)

  (setf (slot-value image 'impl) (make-instance (drivers:driver-image-impl (drivers:active-driver)) :image image)))

(defmethod print-object ((image image) stream)
  (print-unreadable-object (image stream :type t)
    (format stream  "~S" (image-path image))))

(defun image-width (image)
  (drivers:image-width (image-impl image)))

(defun image-height (image)
  (drivers:image-height (image-impl image)))
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

(cl:in-package #:sol.ui)

(defclass image (component)
  ((source
    :type media:image
    :initform nil
    :accessor source)
   (stretch
    :type keyword
    :initarg :stretch
    :accessor stretch)
   (strech-direction
    :type keyword
    :initarg :stretch-direction
    :accessor stretch-direction))
  (:default-initargs
   :background media.colors:*transparent*
    :stretch :uniform
    :stretch-direction :both))

(defmethod initialize-instance :after ((comp image)
                                       &key (source nil)
                                         &allow-other-keys)
  (setf (source comp) source))

(defmethod source ((comp image))
  (slot-value comp 'source))

(defmethod (setf source) (new-val (comp image))
  (unless (equal new-val (source comp))
    (etypecase new-val
      ((or media:image null)
       (setf (slot-value comp 'source) new-val))
      ((or pathname string)
       (setf (slot-value comp 'source)
             (make-instance 'media:image :path new-val)))))
  (source comp))

(defmethod load-component ((comp image))
  (declare (ignore comp)))

(defmethod unload-component ((comp image))
  (declare (ignore comp)))

(defmethod measure-override ((comp image) available-width available-height)
  (%measure-arrange-helper comp available-width available-height))

(defmethod arrange-override ((comp image) width height)
  (multiple-value-bind (used-w used-h)
      (%measure-arrange-helper comp width height)
    (values used-w used-h)))

(defmethod draw ((comp image) renderer)
  (unless (or (zerop (actual-width comp))
              (zerop (actual-height comp)))
    (media:render-draw-image renderer (source comp) 0 0)))

(defun %measure-arrange-helper (comp width height)
  (let ((des-w 0)
        (des-h 0))
    (when (source comp)
      (setf des-w (media:image-width (source comp)))
      (setf des-h (media:image-height (source comp)))
      (multiple-value-bind (scale-w scale-h)
          (%scale-factors
           width height
           des-w des-h
           (stretch comp)
           (stretch-direction comp))
        (setf des-w (* des-w scale-w))
        (setf des-h (* des-h scale-h))))
    (values des-w des-h)))

(defun %scale-factors (a-w a-h c-w c-h stretch stretch-direction)
  (let ((factor-w 1)
        (factor-h 1))

    (when (and (or a-w a-h)
               (member stretch '(:uniform :uniform-to-fill :fill)))
      (cond
        ((and a-w a-h)
         (if (zerop c-w)
             (setf factor-w 0)
             (setf factor-w (/ a-w c-w)))
         (if (zerop c-h)
             (setf factor-h 0)
             (setf factor-h (/ a-h c-h)))

         (case stretch
           (:uniform
            (let ((factor (min factor-w factor-h)))
              (setf factor-w factor)
              (setf factor-h factor)))
           (:uniform-to-fill
            (let ((factor (max factor-w factor-h)))
              (setf factor-w factor)
              (setf factor-h factor)))))
        (a-w
         (if (zerop c-w)
             (setf factor-w 0)
             (setf factor-w (/ a-w c-w)))
         (setf factor-h factor-w))
        (a-h
         (if (zerop c-h)
             (setf factor-h 0)
             (setf factor-h (/ a-h c-h)))
         (setf factor-w factor-h)))

      (case stretch-direction
        (:up-only
         (setf factor-w (max factor-w 1))
         (setf factor-h (max factor-h 1)))
        (:down-only
         (setf factor-w (min factor-w 1))
         (setf factor-h (min factor-h 1)))))

    (values factor-w factor-h)))
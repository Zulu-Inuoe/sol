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

(in-package #:sol.ui)

(defclass content-presenter (component)
  ((content
    :type t
    :initform nil)
   (%content-component
    :type (or null component)
    :initform nil
    :accessor %content-component))
  (:default-initargs
   :background media.colors:*transparent*))

(defmethod initialize-instance :after ((comp content-presenter)
                                       &key (content nil)
                                         &allow-other-keys)
  (setf (content comp) content))

(defgeneric content (comp))
(defgeneric (setf content) (value comp))

(defmethod content ((comp content-presenter))
  (slot-value comp 'content))

(defmethod (setf content) (value (comp content-presenter))
  (when-let ((content (%content-component comp)))
    (when-let ((focused-component (focused-component (active-focus-manager))))
      (when (or
             (eq focused-component content)
             (has-parent-p focused-component content))
        (setf (focused-component (active-focus-manager)) nil)))

    (when (is-loaded comp)
      (unload-component content))
    (setf (parent content) nil)
    (setf (%content-component comp) nil))

  (setf (slot-value comp 'content) value)

  (when value
    (let (content)
      (typecase value
        (component
         (setf content value))
        (string
         (setf content
               (make-instance
                'label
                :text value)))
        (media:image
         (setf content
               (make-instance
                'image
                :source value)))
        (t
         (setf content
               (make-instance
                'label
                :text (format nil "~A" value)))))

      (setf (%content-component comp) content)
      (setf (parent content) comp)

      (when (is-loaded comp)
        (load-component content))))
  (invalidate-arrange comp))

(defmethod get-component-at-* ((comp content-presenter) x y)
  (if (and (>= x 0)
           (>= y 0)
           (< x (actual-width comp))
           (< y (actual-height comp))
           (visible comp)
           (%content-component comp))
      (multiple-value-bind (c c-x c-y)
          (get-component-at-*
           (%content-component comp)
           (- x (x (%content-component comp)))
           (- y (y (%content-component comp))))
        (if c
            (values c c-x c-y)
            nil))
      nil))

(defmethod load-component ((comp content-presenter))
  (when-let ((content (%content-component comp)))
    (load-component content)))

(defmethod unload-component ((comp content-presenter))
  (when-let ((content (%content-component comp)))
    (unload-component content)))

(defmethod measure-override ((comp content-presenter) available-width available-height)
  (if-let ((content (%content-component comp)))
    (measure content available-width available-height)
    (values 0 0)))

(defmethod arrange-override ((comp content-presenter) width height)
  (when-let ((content (%content-component comp)))
    (arrange content 0 0 width height))
  (values width height))

(defmethod draw ((comp content-presenter) renderer)
  (when-let ((content (%content-component comp)))
    (draw content renderer)))

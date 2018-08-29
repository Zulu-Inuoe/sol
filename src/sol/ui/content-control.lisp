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

(defclass content-control (component)
  ((horizontal-content-alignment
    :type keyword
    :initform :center
    :initarg :horizontal-content-alignment
    :accessor horizontal-content-alignment)
   (vertical-content-alignment
    :type keyword
    :initform :center
    :initarg :vertical-content-alignment
    :accessor vertical-content-alignment)
   (%presenter
    :type content-presenter
    :reader %presenter))
  (:default-initargs
   :background media.colors:*transparent*))

(defmethod initialize-instance :after ((comp content-control)
                                       &key (content nil)
                                         &allow-other-keys)
  (setf (slot-value comp '%presenter)
        (make-instance
         'content-presenter
         :parent comp
         :content content
         :horizontal-alignment (horizontal-content-alignment comp)
         :vertical-alignment (vertical-content-alignment comp))))

(defmethod content ((comp content-control))
  (content (%presenter comp)))

(defmethod (setf content) (value (comp content-control))
  (unless (eq value (content (%presenter comp)))
    (setf (content (%presenter comp)) value)))

(defmethod (setf horizontal-content-alignment) (value (comp content-control))
  (setf (slot-value comp 'horizontal-content-alignment) value)
  (setf (horizontal-alignment (%presenter comp)) value))

(defmethod (setf vertical-content-alignment) (value (comp content-control))
  (setf (slot-value comp 'vertical-content-alignment) value)
  (setf (vertical-alignment (%presenter comp)) value))

(defmethod get-component-at-* ((comp content-control) x y)
  (if (and (>= x 0)
           (>= y 0)
           (< x (actual-width comp))
           (< y (actual-height comp))
           (visible comp))
      (multiple-value-bind (c c-x c-y)
          (get-component-at-*
           (%presenter comp)
           (- x (x (%presenter comp)))
           (- y (y (%presenter comp))))
        (if c
            (values c c-x c-y)
            (values comp x y)))
      nil))

(defmethod load-component ((comp content-control))
  (load-component (%presenter comp)))

(defmethod unload-component ((comp content-control))
  (unload-component (%presenter comp)))

(defmethod measure-override ((comp content-control) available-width available-height)
  (measure (%presenter comp) available-width available-height))

(defmethod arrange-override ((comp content-control) width height)
  (arrange (%presenter comp) 0 0 width height)
  (values width height))

(defmethod draw ((comp content-control) renderer)
  (draw (%presenter comp) renderer))

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

(defclass shape (component)
  ((fill
    :type (or null media:color)
    :initarg :fill
    :accessor shape-fill)
   (stroke
    :type (or null media:color)
    :initarg :stroke
    :accessor shape-stroke)
   (stroke-thickness
    :type real
    :initarg :stroke-thickness
    :accessor shape-stroke-thickness))
  (:default-initargs
   :background media.colors:*transparent*
    :fill nil
    :stroke nil
    :stroke-thickness 1))

(defclass line (shape)
  ((x1
    :type real
    :initarg :x1
    :accessor x1)
   (y1
    :type real
    :initarg :y1
    :accessor y1)
   (x2
    :type real
    :initarg :x2
    :accessor x2)
   (y2
    :type real
    :initarg :y2
    :accessor y2))
  (:default-initargs
   :x1 0 :y1 0
   :x2 0 :y2 0))

(defmethod measure-override ((comp line) available-width available-height)
  (let ((des-w (max (x1 comp) (x2 comp)))
        (des-h (max (y1 comp) (y2 comp))))
    (when available-width
      (setf des-w (min des-w available-width)))
    (when available-height
      (setf des-h (min des-h available-height)))

    (values des-w des-h)))

(defmethod draw ((comp line) renderer)
  (media:render-draw-line
   renderer
   (x1 comp)
   (y1 comp)
   (x2 comp)
   (y2 comp)
   :color (shape-stroke comp)
   :thickness (shape-stroke-thickness comp)))

(defclass rectangle (shape)
  ())

(defmethod draw ((comp rectangle) renderer)
  (media:render-draw-rect
   renderer
   0 0
   (actual-width comp) (actual-height comp)
   :fill (shape-fill comp)
   :stroke (shape-stroke comp)
   :stroke-thickness (shape-stroke-thickness comp)))

(defclass ellipse (shape)
  ())

(defmethod draw ((comp ellipse) renderer)
  (let ((width/2 (/ (actual-width comp) 2))
        (height/2 (/ (actual-height comp) 2)))
    (media:render-draw-ellipse
     renderer
     width/2 height/2
     width/2 height/2
     :fill (shape-fill comp)
     :stroke (shape-stroke comp)
     :stroke-thickness (shape-stroke-thickness comp))))

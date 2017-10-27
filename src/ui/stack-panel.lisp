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

(defclass stack-panel (panel)
  ((orientation
    :type keyword
    :initarg :orientation
    :accessor orientation)
   (spacing
    :type number
    :initarg :spacing
    :accessor spacing))
  (:default-initargs
   :orientation :horizontal
    :spacing 0))

(defmethod measure-override ((comp stack-panel) available-width available-height)
  (let ((des-w 0)
        (des-h 0))
    (ecase (orientation comp)
      (:horizontal
       (dolist (child (children comp))
         (multiple-value-bind (c-w c-h) (measure child nil available-height)
           (incf des-w (+ c-w (spacing comp)))
           (setf des-h (max des-h c-h))))
       (when (children comp)
         (decf des-w (spacing comp))))
      (:vertical
       (dolist (child (children comp))
         (multiple-value-bind (c-w c-h) (measure child available-width nil)
           (setf des-w (max des-w c-w))
           (incf des-h (+  c-h (spacing comp)))))
       (when (children comp)
         (decf des-h (spacing comp)))))

    (values des-w des-h)))

(defmethod arrange-override ((comp stack-panel) width height)
  (let ((x 0)
        (y 0))
    (ecase (orientation comp)
      (:horizontal
       (dolist (child (children comp))
         (arrange child x y (desired-width child) (desired-height child))
         (incf x (+ (desired-width child) (spacing comp)))))
      (:vertical
       (dolist (child (children comp))
         (arrange child x y (desired-width child) (desired-height child))
         (incf y (+ (desired-height child) (spacing comp))))))

    (values width height)))
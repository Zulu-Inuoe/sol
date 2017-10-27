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

(defclass grid (panel)
  ())

(defmethod measure-override ((comp grid) available-width available-height)
  (let ((des-w 0)
        (des-h 0))
    (dolist (child (children comp))
      (multiple-value-bind (c-w c-h)
          (measure child available-width available-height)
        (setf des-w (max des-w c-w))
        (setf des-h (max des-h c-h))))
    (values des-w des-h)))

(defmethod arrange-override ((comp grid) width height)
  (dolist (child (children comp))
    (arrange child 0 0 width height))
  (values width height))
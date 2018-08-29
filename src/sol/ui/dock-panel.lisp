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

(defclass dock-panel (panel)
  ()
  (:default-initargs))

(defmethod measure-override ((comp dock-panel) available-width available-height)
  (let ((des-w 0)
        (des-h 0)
        (right 0)
        (bottom 0))
    (dolist (child (children comp))
      (multiple-value-bind (c-w c-h)
          (measure child
                   (if (null available-width)
                       nil
                       (max 0 (- available-width right)))
                   (if (null available-height)
                       nil
                       (max 0 (- available-height bottom))))
        (ecase (dock-panel.dock child)
          ((:left :right)
           (setf des-h (max des-h (+ bottom c-h)))
           (incf right c-w))
          ((:top :bottom)
           (setf des-w (max des-w (+ right c-w)))
           (incf bottom c-h)))))
    (setf des-w (max des-w right))
    (setf des-h (max des-h bottom))

    (values des-w des-h)))

(defmethod arrange-override ((comp dock-panel) width height)
  (declare (optimize (debug 3)))
  (let ((x 0) (right 0)
        (y 0) (bottom 0))
    (dolist (child (butlast (children comp)))
      (let ((cx x) (cy y)
            (cw (max 0 (- width (+ x right))))
            (ch (max 0 (- height (+ y bottom)))))
        (ecase (dock-panel.dock child)
          (:left
           (incf x (desired-width child))
           (setf cw (desired-width child)))
          (:right
           (incf right (desired-width child))
           (setf cx (max 0 (- width right)))
           (setf cw (desired-width child)))
          (:top
           (incf y (desired-height child))
           (setf ch (desired-height child)))
          (:bottom
           (incf bottom (desired-height child))
           (setf cy (max 0 (- height bottom)))
           (setf ch (desired-height child))))
        (arrange child cx cy cw ch)))

    (dolist (child (last (children comp)))
      (arrange child x y (max 0 (- width (+ x right))) (max 0 (- height (+ y bottom)))))

    (values width height)))

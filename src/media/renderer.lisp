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

(defmacro render-with-target ((renderer new-target) &body body)
  (once-only (renderer)
    (with-gensyms (old-target)
      `(let* ((,old-target (render-get-target ,renderer)))
         (render-set-target ,renderer ,new-target)
         (unwind-protect
              (progn ,@body)
           (render-set-target ,renderer ,old-target))))))

(defmacro render-with-new-target ((renderer &rest target-params) &body body)
  (once-only (renderer)
    (with-gensyms (new-target successful)
      `(let ((,new-target (render-create-target ,renderer ,@target-params))
             (,successful nil))
         (unwind-protect
              (progn
                (render-with-target (,renderer ,new-target)
                  ,@body)
                (setf ,successful t)
                ,new-target)
           (unless ,successful
             (render-destroy-target ,new-target)))))))

(defgeneric renderer-size (renderer))

(defgeneric renderer-width (renderer)
  (:method (renderer)
    (nth-value 0 (renderer-size renderer))))

(defgeneric renderer-height (renderer)
  (:method (renderer)
    (nth-value 1 (renderer-size renderer))))

(defgeneric render-clear (renderer &key color))
(defgeneric render-present (renderer))

(defgeneric render-pop (renderer))
(defgeneric render-push-translate (renderer x y))
(defgeneric render-push-rotate (renderer angle &key x y))
(defgeneric render-push-scale (renderer scale-x scale-y))

(defgeneric render-draw-point (renderer x y &key color))
(defgeneric render-draw-line (renderer x1 y1 x2 y2 &key color thickness))
(defgeneric render-draw-rect (renderer x y width height &key fill stroke stroke-thickness))
(defgeneric render-draw-ellipse (renderer x y rx ry &key fill stroke stroke-thickness))
(defgeneric render-draw-image (renderer image x y &key width height flip))
(defgeneric render-draw-text (renderer text x y))

(defgeneric render-create-target (renderer width height &key opacity))
(defgeneric render-get-target (renderer))
(defgeneric render-set-target (renderer target))
(defgeneric render-draw-target (renderer target x y))
(defgeneric render-destroy-target (renderer target))
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

(cl:in-package :sol.ui)

(defun capturing-component ()
  *%capturing-component*)

(defun (setf capturing-component) (new-val)
  (let ((old-val (capturing-component)))
    (unless (eq new-val old-val)
      (setf *%capturing-component* new-val)
      (cond
        ((null new-val)
         (input:release-mouse))
        ((null old-val)
         (input:capture-mouse)))

      (if new-val
          (setf (mouse-over-component) new-val)
          ;;TODO
          ;; (setf (mouse-over-component)
          ;;       (multiple-value-bind (x y)
          ;;           (input:mouse-position)
          ;;         (dolist (w (hash-table-values (windows (current-ui-manager))))
          ;;           (let ((comp (get-component-at-* w (- x (window-left w)) (- y (window-top w)))))
          ;;             (when comp
          ;;               (return comp))))))
          )))
  (capturing-component))

(defparameter *%capturing-component* nil)
(defparameter *%mouse-over-component* nil)

(defun mouse-over-component ()
  *%mouse-over-component*)

(defun (setf mouse-over-component) (new-val)
  (let ((old-val (mouse-over-component)))
    (unless (eq new-val old-val)
      (setf *%mouse-over-component* new-val)

      (when old-val
        (dolist (p (list-parents
                    old-val
                    (and new-val
                         (lambda (p)
                           ;;Stop if we stumble on either the new component
                           ;;or a component that is a parent of the new component
                           (or (eq p new-val)
                               (has-parent-p new-val p))))))
          (raise-event
           p
           'e_mouse-leave
           (make-instance 'routed-event-args :source p))))

      ;;Raise events in the new component's parents
      (when new-val
        (dolist (p (nreverse (list-parents
                              new-val
                              (and old-val
                                   ;;Stop if we stumble on the old component
                                   ;;everything above it is already up to date
                                   ;;or a if we hit one that is a parent of the
                                   ;;old component since that is also up to date
                                   (lambda (p)
                                     (or (eq p old-val)
                                         (has-parent-p old-val p)))))))
          (raise-event
           p
           'e_mouse-enter
           (make-instance 'routed-event-args :source p))))))
  (mouse-over-component))
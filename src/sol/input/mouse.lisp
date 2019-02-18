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

(in-package #:sol.input)

(defvar *%mouse-cursors* (make-hash-table))

(defun get-mouse-cursor ()
  ;; TODO
  ;; (let ((cur (sdl2:sdl-get-cursor)))
  ;;   (when (cffi:pointer-eq cur (sdl2:sdl-get-default-cursor))
  ;;     (return-from get-mouse-cursor :default))
  ;;   (loop
  ;;      :for k :being :the :hash-keys :in *%mouse-cursors*
  ;;      :using (hash-value v)
  ;;      :when (cffi:pointer-eq cur v)
  ;;      :do (return-from get-mouse-cursor k))
  ;;   :custom)
  )


(defun set-mouse-cursor (cursor)
  (declare (ignore cursor))
  ;; TODO
  ;; (case cursor
  ;;   (:default
  ;;    (progn
  ;;      (sdl2:sdl-set-cursor (sdl2:sdl-get-default-cursor))
  ;;      (sdl2:sdl-show-cursor sdl2:+sdl-enable+)))
  ;;   (:none
  ;;    (sdl2:sdl-show-cursor sdl2:+sdl-disable+))
  ;;   (t
  ;;    (if-let ((sdl-cursor (gethash cursor *%mouse-cursors*)))
  ;;      (progn
  ;;        (sdl2:sdl-set-cursor sdl-cursor)
  ;;        (sdl2:sdl-show-cursor sdl2:+sdl-enable+))
  ;;      (sdl2:sdl-show-cursor sdl2:+sdl-disable+))))
  (values))

(defun capture-mouse ()
  ;; TODO
  ;; (sdl2:sdl-capture-mouse 1)
  (values))

(defun release-mouse ()
  ;; TODO
  ;; (sdl2:sdl-capture-mouse 0)
  (values))

(defun mouse-position ()
  ;; TODO
  ;; (cffi:with-foreign-objects ((x :int)
  ;;                             (y :int))
  ;;   (sdl2:sdl-get-global-mouse-state x y)
  ;;   (values (cffi:mem-ref x :int) (cffi:mem-ref y :int)))
  )

(defun %mouse-init ()
  ;; TODO
  ;; (setf (gethash :arrow *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-arrow+)
  ;;       (gethash :cross *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-crosshair+)
  ;;       (gethash :hand *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-hand+)
  ;;       (gethash :ibeam *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-ibeam+)
  ;;       (gethash :size-nesw *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-sizenesw+)
  ;;       (gethash :size-ns *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-sizens+)
  ;;       (gethash :size-nwse *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-sizenwse+)
  ;;       (gethash :size-we *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-sizewe+)
  ;;       (gethash :wait *%mouse-cursors*)
  ;;       (sdl2:sdl-create-system-cursor sdl2:+sdl-system-cursor-wait+))
  )

(defun %mouse-uninit ()
  ;; TODO
  ;; (maphash-values
  ;;  (lambda (v)
  ;;    (sdl2:sdl-free-cursor v))
  ;;  *%mouse-cursors*)
  (clrhash *%mouse-cursors*))

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

(defun get-mouse-cursor ()
  (let ((cur (sdl2-ffi.functions:sdl-get-cursor)))
    (when (= (cffi:pointer-address (autowrap:ptr cur))
             (cffi:pointer-address (autowrap:ptr (sdl2-ffi.functions:sdl-get-default-cursor))))
      (return-from get-mouse-cursor :default))
    (loop
       :for k :being :the :hash-keys :in *%mouse-cursors*
       :using (hash-value v)
       :when (=
              (cffi:pointer-address (autowrap:ptr cur))
              (cffi:pointer-address (autowrap:ptr v)))
       :do (return-from get-mouse-cursor k))
    :custom))

(defun set-mouse-cursor (cursor)
  (case cursor
    (:default
     (progn
       (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-get-default-cursor))
       (sdl2-ffi.functions:sdl-show-cursor sdl2-ffi:+sdl-enable+)))
    (:none
     (sdl2-ffi.functions:sdl-show-cursor sdl2-ffi:+sdl-disable+))
    (t
     (if-let ((sdl-cursor (gethash cursor *%mouse-cursors*)))
       (progn
         (sdl2-ffi.functions:sdl-set-cursor sdl-cursor)
         (sdl2-ffi.functions:sdl-show-cursor sdl2-ffi:+sdl-enable+))
       (sdl2-ffi.functions:sdl-show-cursor sdl2-ffi:+sdl-disable+))))
  (values))

(defun capture-mouse ()
  (sdl2-ffi.functions:sdl-capture-mouse 1)
  (values))

(defun release-mouse ()
  (sdl2-ffi.functions:sdl-capture-mouse 0)
  (values))

(defun mouse-position ()
  (cffi:with-foreign-objects ((x :int)
                              (y :int))
    (sdl2-ffi.functions:sdl-get-global-mouse-state x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(defvar *%mouse-cursors* (make-hash-table))

(defun %mouse-init ()
  (setf (gethash :arrow *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-arrow+))
  (setf (gethash :cross *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-crosshair+))
  (setf (gethash :hand *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-hand+))
  (setf (gethash :ibeam *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-ibeam+))
  (setf (gethash :size-nesw *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-sizenesw+))
  (setf (gethash :size-ns *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-sizens+))
  (setf (gethash :size-nwse *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-sizenwse+))
  (setf (gethash :size-we *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-sizewe+))
  (setf (gethash :wait *%mouse-cursors*)
        (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-wait+)))

(defun %mouse-uninit ()
  (maphash-values
   (lambda (v)
     (sdl2-ffi.functions:sdl-free-cursor v))
   *%mouse-cursors*)
  (clrhash *%mouse-cursors*))
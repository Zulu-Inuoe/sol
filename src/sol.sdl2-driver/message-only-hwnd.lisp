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

(in-package #:sol.sdl2-driver)

(defun %make-guid ()
  "Create a randomly-generated 128-bit GUID in the form of a {} wrapped, 32-digit, hyphen-separated, hexadecimal string.
  Example: {A3C78807-EA6D-A4AE-1CCA-797A6BF88C31}"
  (let ((value (random (ash 1 128))))
    (format nil "{~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X}"
            (ldb (byte 32 96) value)
            (ldb (byte 16 80) value)
            (ldb (byte 16 64) value)
            (ldb (byte 16 48) value)
            (ldb (byte 48 0) value))))

(defun %memset (ptr value num)
  "A wicked slow memset"
  (dotimes (i num)
    (setf (cffi:mem-aref ptr :int8 i) value)))

(defclass %message-only-hwnd (disposeable)
  ((%hwnd
    :reader %hwnd)
   (%class-atom
    :reader %class-atom)
   (%wndproc
    :type cffi:foreign-pointer
    :initarg :wndproc
    :initform (cffi:foreign-symbol-pointer "DefWindowProcW")
    :reader %wndproc)))

(defmethod initialize-instance :after ((this %message-only-hwnd) &key &allow-other-keys)
  (let* ((wndclass-name (format nil "MessageOnlyHwnd[~A(~A);~A;~A]"
                                (lisp-implementation-type)
                                (lisp-implementation-version)
                                (bordeaux-threads:thread-name (bordeaux-threads:current-thread))
                                (%make-guid))))
    (cffi:with-foreign-object (class 'win32:wndclassex)
      (%memset class 0 (cffi:foreign-type-size 'win32:wndclassex))
      (cffi:with-foreign-slots ((win32:cbsize win32:wndproc win32:instance win32:wndclass-name) class win32:wndclassex)
        (setf win32:cbsize (cffi:foreign-type-size 'win32:wndclassex)
              win32:wndproc (%wndproc this)
              win32:instance (win32:get-module-handle (cffi:null-pointer))
              win32:wndclass-name wndclass-name))
      (setf (slot-value this '%class-atom) (win32:register-class-ex class)
            (slot-value this '%hwnd)
            (win32:create-window-ex
             0
             wndclass-name
             wndclass-name
             0
             0 0 0 0
             (cffi:make-pointer win32:+hwnd-message+)
             (cffi:null-pointer)
             (cffi:null-pointer)
             (cffi:null-pointer))))))

(defmethod dispose ((obj %message-only-hwnd))
  (win32:destroy-window (%hwnd obj))
  (win32:unregister-class (cffi:make-pointer (%class-atom obj)) (cffi:null-pointer))
  (slot-makunbound obj '%hwnd)
  (slot-makunbound obj '%class-atom)
  (call-next-method))

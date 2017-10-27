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

(defclass font ()
  ((font-family
    :documentation "The family name of this font"
    :type string
    :initarg :family
    :initform "Segoe UI"
    :reader font-family)
   (font-style
    :documentation "The style of this font (Regular, Bold, Italic, etc.)"
    :type string
    :initarg :style
    :initform "Regular"
    :reader font-style)
   (font-size
    :documentation "The point size of this font"
    :type :integer
    :initarg :size
    :initform 12
    :reader font-size)
   (font-bold
    :documentation "Whether or not this font is bold"
    :type boolean
    :initarg :bold
    :initform nil
    :reader font-bold)
   (font-italic
    :documentation "Whether or not this font is italic"
    :type boolean
    :initarg :italic
    :initform nil
    :reader font-italic)
   (font-underline
    :documentation "Whether or not this font is underlined"
    :type boolean
    :initarg :underline
    :initform nil
    :reader font-underline)
   (font-strikethrough
    :documentation "Whether or not this font uses the strikethrough effect"
    :type boolean
    :initarg :strikethrough
    :initform nil
    :reader font-strikethrough)
   (%ttf-font
    :type sdl2-ffi:ttf-font))
  (:documentation
   "Represents font information"))

(defmethod print-object ((f font) stream)
  (print-unreadable-object (f stream :type t)
    (format stream  "~A ~A ~A" (font-family f) (font-style f) (font-size f))))

(defvar *default-font* (make-instance 'font))

(defun font-line-height (font)
  "Get the height of a line of text in the given 'font'"
  (let ((ttf-font (%font-ttf-font font)))
    (sdl2-ffi.functions:ttf-font-height ttf-font)))

(defun font-size-text (font text)
  "Get the width and height of the given `text' according to `font', when rendered as a straight line."
  (let ((ttf-font (%font-ttf-font font)))
    (cffi:with-foreign-objects ((w :int)
                                (h :int))
      (sdl2-ffi.functions:ttf-size-utf8 ttf-font text w h)
      (values (cffi:mem-ref w :int) (cffi:mem-ref h :int)))))

(defun %font-ttf-font (font)
  (unless (slot-boundp font '%ttf-font)
    (setf (slot-value font '%ttf-font)
          (%load-font font))
    (push font (%live-fonts (%font-context))))

  (slot-value font '%ttf-font))

(defclass %font-context ()
  ((%loaded-font-files
    :type hash-table
    :documentation "Path <=> ttf-font. keeps track of available font types"
    :initform (make-hash-table :test 'equal)
    :reader %loaded-font-files)
   (%loaded-fonts
    :type hash-table
    :documentation "(font-family font-style font-point-size bold italic underline strikethrough) <=> ttf-font"
    :initform (make-hash-table :test 'equalp)
    :reader %loaded-fonts)
   (%live-fonts
    :type list
    :documentation "List of font objects that are referencing a ttf font"
    :initform nil
    :accessor %live-fonts)))

(defvar *%font-context* nil)

(defun %font-context ()
  (media-init)
  *%font-context*)

(defun %font-context-destroy (context)
  (dolist (f (%live-fonts context))
    (slot-makunbound f '%ttf-font))
  (setf (%live-fonts context) nil)
  (maphash
   (lambda (k v)
     (format t "font-context: closing font ~A~%" k)
     (sdl2-ttf:close-font v))
   (%loaded-font-files context))
  (clrhash (%loaded-font-files context))
  (maphash
   (lambda (k v)
     (format t "font-context: closing font ~A~%" k)
     (sdl2-ttf:close-font v))
   (%loaded-fonts context))
  (clrhash (%loaded-fonts context)))

#+win32
(progn
  (cffi:load-foreign-library "Advapi32.dll")

  (defconstant +standard-rights-read+ #x00020000)
  (defconstant +key-query-value+ #x0001)
  (defconstant +key-enumerate-sub-keys+ #x0008)
  (defconstant +key-notify+ #x0010)
  (defconstant +key-read+ (logior +standard-rights-read+ +key-query-value+ +key-enumerate-sub-keys+ +key-notify+))

  (defconstant +hkey-classes-root+ #x80000000)
  (defconstant +hkey-current-user+ #x80000001)
  (defconstant +hkey-local-machine+ #x80000002)

  (defconstant +rrf-rt-reg-sz+ #x00000002)

  (cffi:defcfun ("RegGetValueW" reg-get-value) :int32
    (hkey :pointer)
    (sub-key (:string :encoding #+little-endian :utf-16le #+big-endian :utf-16be))
    (value-name (:string :encoding #+little-endian :utf-16le #+big-endian :utf-16be))
    (flags :uint32)
    (type :pointer)
    (data :pointer)
    (data-size :pointer))

  (defun %get-font-substitute (font-name)
    (cffi:with-foreign-object (psize :uint32)
      (when (zerop (reg-get-value (cffi:make-pointer +hkey-local-machine+)
                                  "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\FontSubstitutes"
                                  font-name
                                  +rrf-rt-reg-sz+
                                  (cffi:null-pointer)
                                  (cffi:null-pointer)
                                  psize))
        (cffi:with-foreign-object (buffer :uint8 (cffi:mem-ref psize :uint32))
          (when (zerop
                 (reg-get-value (cffi:make-pointer +hkey-local-machine+)
                                "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\FontSubstitutes"
                                font-name
                                +rrf-rt-reg-sz+
                                (cffi:null-pointer)
                                buffer
                                psize))
            (cffi:foreign-string-to-lisp buffer :encoding #+little-endian :utf-16le #+big-endian :utf-16be)))))))

(defun %find-font-file (context family style)
  (loop
     :for k :being :the :hash-keys :in (%loaded-font-files context)
     :using (hash-value v)
     :if (and (string-equal
               (sdl2-ffi.functions:ttf-font-face-family-name v)
               family)
              (string-equal
               (sdl2-ffi.functions:ttf-font-face-style-name v)
               style))
     :do (return-from %find-font-file k))

  ;;Try searching in the fonts directory
  #+win32
  (dolist (p (uiop/filesystem:directory-files
              (concatenate 'string (uiop/os:getenv "WINDIR") "\\fonts\\")
              "*.ttf"))
    (unless (gethash p (%loaded-font-files context))
      (let ((font (sdl2-ttf:open-font (namestring p) 8)))
        (setf (gethash p (%loaded-font-files context)) font)
        (when (and (string-equal
                    (sdl2-ffi.functions:ttf-font-face-family-name font)
                    family)
                   (string-equal
                    (sdl2-ffi.functions:ttf-font-face-style-name font)
                    style))
          (return-from %find-font-file p)))))

  #+win32
  ;;On Windows certain font names are substitutes for other fonts
  ;;Search for the substitute font
  (when-let* ((substitute (%get-font-substitute family))
              (f (%find-font-file context substitute style)))
    (return-from %find-font-file f))

  ;;If nothing else then fallback to some font we have available
  (loop
     :for k :being :the :hash-keys :in (%loaded-font-files context)
     :return k))

(defun %load-font (font
                   &aux
                     (context (%font-context))
                     (family (font-family font))
                     (style (font-style font))
                     (point-size (font-size font))
                     (bold (font-bold font))
                     (italic (font-italic font))
                     (underline (font-underline font))
                     (strikethrough (font-strikethrough font)))
  (let ((ttf-font
         (gethash (list family style point-size bold italic underline strikethrough) (%loaded-fonts context))))
    (unless ttf-font
      (let ((font-path (%find-font-file context family style)))
        (unless font-path
          (error "font: unavailable font family and style: ~A ~A" family style))

        (setf ttf-font
              (sdl2-ttf:open-font
               (namestring font-path)
               point-size))
        (sdl2-ffi.functions:ttf-set-font-style
         ttf-font
         (%font-properties-to-bitmask bold italic underline strikethrough))

        (setf (gethash (list family style point-size bold italic underline strikethrough) (%loaded-fonts context)) ttf-font)))
    ttf-font))

(defun %font-properties-to-bitmask (bold italic underline strikethrough)
  (logior
   (if bold sdl2-ffi:+ttf-style-bold+ 0)
   (if italic sdl2-ffi:+ttf-style-italic+ 0)
   (if underline sdl2-ffi:+ttf-style-underline+ 0)
   (if strikethrough sdl2-ffi:+ttf-style-strikethrough+ 0)))
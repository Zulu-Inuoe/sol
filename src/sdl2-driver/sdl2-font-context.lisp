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

(defvar *%font-context* nil)

(defclass sdl2-font-context ()
  ((%loaded-font-files
    :type hash-table
    :documentation "Path <=> ttf-font. keeps track of available font types"
    :initform (make-hash-table :test 'equal)
    :reader %loaded-font-files)
   (%loaded-fonts
    :type hash-table
    :documentation "(font-family font-style font-point-size bold italic underline strikethrough) <=> ttf-font"
    :initform (make-hash-table :test 'equalp)
    :reader %loaded-fonts))
  (:documentation
   "Cache for available fonts. Fonts are immutable and thus trivially reusable."))

(defmethod initialize-instance :after ((context sdl2-font-context) &key &allow-other-keys)
  (unless (null *%font-context*)
    (error "sdl2-font-context: context already exists"))
  (setf *%font-context* context))

(defmethod dispose ((context sdl2-font-context))
  (unless (eq *%font-context* context)
    (error "sdl2-font-context: multiple contexts"))
  (setf *%font-context* nil)
  (maphash
   (lambda (k v)
     (format t "sdl2-font-context: closing font ~A~%" k)
     (sdl2-ttf:close-font v))
   (%loaded-font-files context))
  (clrhash (%loaded-font-files context))
  (maphash
   (lambda (k v)
     (format t "sdl2-font-context: closing font ~A~%" k)
     (sdl2-ttf:close-font v))
   (%loaded-fonts context))
  (clrhash (%loaded-fonts context)))

#+win32
(defun %get-font-substitute (font-name)
  (cffi:with-foreign-object (psize :uint32)
    (when (zerop (win32:reg-get-value (cffi:make-pointer win32:+hkey-local-machine+)
                                      "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\FontSubstitutes"
                                      font-name
                                      win32:+rrf-rt-reg-sz+
                                      (cffi:null-pointer)
                                      (cffi:null-pointer)
                                      psize))
      (cffi:with-foreign-object (buffer :uint8 (cffi:mem-ref psize :uint32))
        (when (zerop
               (win32:reg-get-value (cffi:make-pointer win32:+hkey-local-machine+)
                              "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\FontSubstitutes"
                              font-name
                              win32:+rrf-rt-reg-sz+
                              (cffi:null-pointer)
                              buffer
                              psize))
          (values (cffi:foreign-string-to-lisp buffer :encoding win32:+win32-string-encoding+)))))))

(defun %slurp-file (path)
  (with-output-to-string (str)
    (with-open-file (stream path)
      (loop
         :for line := (read-line stream nil nil)
         :while line
         :doing
         (write-line #+win32(string-right-trim '(#\return) line) #-win32 line str)))))

#+win32
(defun %font-directories ()
  (list
   (parse-namestring (concatenate 'string (uiop/os:getenv "WINDIR") "\\fonts\\"))))

#-win32
(defun %font-directories ()
  (cond
    ((probe-file "/etc/fonts/fonts.conf")
     (let ((font-conf (xmls:parse (%slurp-file "/etc/fonts/fonts.conf"))))
       (loop :for dir-node :in (xmls:xmlrep-find-child-tags "dir" font-conf)
          :for path := (xmls:xmlrep-string-child dir-node)
          :when (uiop:directory-exists-p path)
          :collect (uiop:ensure-directory-pathname path))))
    (t
     (loop :for path :in '(#p"/usr/share/fonts/"
                           #p"/usr/local/share/fonts/"
                           #p"~/.fonts/")
          :when (uiop:directory-exists-p path)
        :collect (uiop:ensure-directory-pathname path)))))

(defun %load-font-file (context path)
  (let ((font (sdl2-ffi.functions:ttf-open-font (namestring path) 8)))
    (setf (gethash path (%loaded-font-files context)) font)
    font))

(defun %font-matches (font family style)
  (and (string-equal
        (sdl2-ffi.functions:ttf-font-face-family-name font)
        family)
       (string-equal
        (sdl2-ffi.functions:ttf-font-face-style-name font)
        style)))

(defun %find-font-file (context family style)
  (loop
     :for k :being :the :hash-keys :in (%loaded-font-files context)
     :using (hash-value v)
     :if (%font-matches v family style)
     :do (return-from %find-font-file k))

  ;;Try searching in the fonts directories
  (labels ((recurse (dir)
             (dolist (p (uiop/filesystem:directory-files dir))
               (unless (or
                        (not (member (pathname-type p) '("ttf" "otf") :test 'string-equal))
                        (gethash p (%loaded-font-files context)))
                 (when (%font-matches (%load-font-file context p) family style)
                   (return-from %find-font-file p))))
             (dolist (d (uiop/filesystem:subdirectories dir))
               (recurse d))))
    (dolist (d (%font-directories))
      (recurse d)))

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
                     (context *%font-context*)
                     (family (media:font-family font))
                     (style (media:font-style font))
                     (point-size (media:font-size font))
                     (bold (media:font-bold font))
                     (italic (media:font-italic font))
                     (underline (media:font-underline font))
                     (strikethrough (media:font-strikethrough font)))
  (let ((ttf-font
         (gethash (list family style point-size bold italic underline strikethrough) (%loaded-fonts context))))
    (unless ttf-font
      (let ((font-path (%find-font-file context family style)))
        (unless font-path
          (error "sdl2-font-context: unavailable font family and style: ~A ~A" family style))

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
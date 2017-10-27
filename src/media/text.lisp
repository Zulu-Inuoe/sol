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

(defclass text ()
  ((text-string
    :type string
    :initarg :string
    :initform ""
    :reader text-string)
   (text-font
    :type font
    :initarg :font
    :initform *default-font*
    :reader text-font)
   (text-color
    :type color
    :initarg :color
    :initform *black*
    :reader text-color)
   (text-width
    :type integer
    :reader text-width)
   (text-height
    :type integer
    :reader text-height)
   (%text-sdl-surface-ptr
    :type cons
    :initform (cons nil nil)
    :reader %text-sdl-surface-ptr)))

(defmethod print-object ((text text) stream)
  (print-unreadable-object (text stream :type t)
    (format stream "\"~A\"" (text-string text))))

(defmethod initialize-instance :after ((text text) &key &allow-other-keys)
  (%render-text text)
  (trivial-garbage:finalize text (%text-make-dispose-fn text)))

(defmethod dispose ((text text))
  (trivial-garbage:cancel-finalization text)
  (funcall (%text-make-dispose-fn text)))

(defun (setf text-string) (value text)
  (when (string-not-equal value (text-string text))
    (setf (slot-value text 'text-string) value)
    (%render-text text))
  value)

(defun (setf text-font) (value text)
  (setf (slot-value text 'text-font) value)
  (%render-text text)
  value)

(defun (setf text-color) (value text)
  (setf (slot-value text 'text-color) value)
  (%render-text text)
  value)

(defun %text-sdl-surface (text)
  (car (%text-sdl-surface-ptr text)))

(defun (setf %text-sdl-surface) (value text)
  (setf (car (%text-sdl-surface-ptr text)) value))

(defun %text-make-dispose-fn (text)
  (let ((surface-ptr (%text-sdl-surface-ptr text)))
    (lambda ()
      (when (car surface-ptr)
        (sdl2:free-surface (car surface-ptr))
        (setf (car surface-ptr) nil)))))

(defun %render-text (text
                     &aux
                       (text-string (text-string text))
                       (font (text-font text))
                       (color (text-color text)))
  (when-let ((old-surface (%text-sdl-surface text)))
    (sdl2:free-surface old-surface)
    (setf (%text-sdl-surface text) nil))

  (when (zerop (length text-string))
    (setf (slot-value text 'text-width) 0)
    (setf (slot-value text 'text-height) 0)
    (return-from %render-text))

  (let ((surface
         (sdl2-ttf:render-utf8-blended
          (%font-ttf-font font)
          text-string
          (r color)
          (g color)
          (b color)
          (a color))))
    (setf (%text-sdl-surface text) surface)
    (setf (slot-value text 'text-width) (sdl2:surface-width surface))
    (setf (slot-value text 'text-height) (sdl2:surface-height surface))))
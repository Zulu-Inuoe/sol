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
   (impl
    :reader font-impl))
  (:documentation
   "Represents font information."))

(defmethod initialize-instance :after ((font font) &key &allow-other-keys)
  (setf (slot-value font 'impl) (make-instance (drivers:font-impl) :font font)))

(defmethod print-object ((f font) stream)
  (print-unreadable-object (f stream :type t)
    (format stream  "~A ~A ~A" (font-family f) (font-style f) (font-size f))))

(defun font-line-height (font)
  "Get the height of a line of text in the given 'font'"
  (drivers:font-height (font-impl font)))

(defun font-size-text (font text)
  "Get the width and height of the given `text' according to `font', when rendered as a straight line."
  (drivers:font-size-text (font-impl font) text))
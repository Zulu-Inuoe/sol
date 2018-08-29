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

(defclass text-component (component)
  ((text-source
    :type media:text
    :accessor text-source)))

(defmethod initialize-instance :after ((comp text-component)
                                       &key
                                         (text "")
                                         &allow-other-keys)
  (setf (text-source comp)
        (make-instance
         'media:text
         :font (make-instance
                'media:font
                :family (font-family comp)
                :style (font-style comp)
                :size (font-size comp))
         :color (foreground comp)
         :string text)))

(defgeneric text (comp))
(defgeneric (setf text) (new-val comp))

(defmethod text ((comp text-component))
  (media:text-string (text-source comp)))

(defmethod (setf text) (val (comp text-component))
  (when (string-not-equal val (media:text-string (text-source comp)))
    (setf (media:text-string (text-source comp)) val))
  val)

(defmethod (setf foreground) (val (comp text-component))
  (setf (slot-value comp 'foreground) val)
  (setf (media:text-color (text-source comp)) val)
  val)

(defmethod (setf font-family) (val (comp text-component))
  (setf (slot-value comp 'font-family) val)
  (setf (media:text-font (text-source comp))
        (make-instance
         'media:font
         :family (font-family comp)
         :style (font-style comp)
         :size (font-size comp))))

(defmethod (setf font-style) (val (comp text-component))
  (setf (slot-value comp 'font-style) val)
  (setf (media:text-font (text-source comp))
        (make-instance
         'media:font
         :family (font-family comp)
         :style (font-style comp)
         :size (font-size comp))))

(defmethod (setf font-size) (val (comp text-component))
  (setf (slot-value comp 'font-size) val)
  (setf (media:text-font (text-source comp))
        (make-instance
         'media:font
         :family (font-family comp)
         :style (font-style comp)
         :size (font-size comp))))

(defmethod load-component ((comp text-component))
  (declare (ignore comp)))

(defmethod unload-component ((comp text-component))
  (declare (ignore comp)))

(defmethod measure-override ((comp text-component) available-width available-height)
  (let ((des-w (media:text-width (text-source comp)))
        (des-h (media:text-height (text-source comp))))
    (when available-width
      (setf des-w (min des-w available-width)))
    (when available-height
      (setf des-h (min des-h available-height)))

    (values des-w des-h)))

(defmethod arrange-override ((comp text-component) width height)
  (values width height))

(defmethod draw ((comp text-component) renderer)
  (unless (or (zerop (actual-width comp))
              (zerop (actual-height comp)))
    (media:render-draw-text renderer (text-source comp) 0 0)))

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

(defclass color ()
  ((color-vector
    :type (simple-array (unsigned-byte 8) 4)
    :reader fp)))

(defmethod print-object ((c color) stream)
  (print-unreadable-object (c stream :type t)
    (format stream  "0x~2,'0X~2,'0X~2,'0X~2,'0X" (r c) (g c) (b c) (a c))))

(defmethod initialize-instance :after ((color color) &key (r 0) (g 0) (b 0) (a #xFF))
  (declare (type real r g b a))
  (setf (slot-value color 'color-vector)
        (make-array
         '(4)
         :element-type '(unsigned-byte 8)
         :initial-contents
         (list
          (logand (round r) #xFF)
          (logand (round g) #xFF)
          (logand (round b) #xFF)
          (logand (round a) #xFF)))))

(defun color (&key (r 0) (g 0) (b 0) (a #xFF))
  (make-instance 'color :a a :r r :g g :b b))

(declaim (ftype (function (color) (unsigned-byte 8)) r g b a))
(defun r (color)
  (aref (fp color) 0))

(defun g (color)
  (aref (fp color) 1))

(defun b (color)
  (aref (fp color) 2))

(defun a (color)
  (aref (fp color) 3))

(defun color= (color1 color2)
  (and (= (r color1) (r color2))
       (= (g color1) (g color2))
       (= (b color1) (b color2))
       (= (a color1) (a color2))))

(defun color-* (color)
  (values (r color) (g color) (b color) (a color)))

(declaim (type (simple-array (unsigned-byte 8) (#.(* 256 256))) +color-avg-table+))
(defparameter +color-avg-table+
  (make-array
   '(#.(* 256 256))
   :element-type '(unsigned-byte 8)
   :initial-contents
   (loop :for x :in (alexandria:iota 256)
      :appending
      (loop :for y :in (alexandria:iota 256)
         :collecting
         (round
          (sqrt (/ (+ (* x x) (* y y)) 2))))))
  "A lookup table that has all color component averages. Access at [x][y] to get average of x and y.
It is a linear table for reduced space and faster access times.")

(defun color-avg (c1 c2)
  (color
   :r (aref +color-avg-table+ (logior (ash (r c1) 8) (r c2)))
   :g (aref +color-avg-table+ (logior (ash (g c1) 8) (g c2)))
   :b (aref +color-avg-table+ (logior (ash (b c1) 8) (b c2)))
   :a (aref +color-avg-table+ (logior (ash (a c1) 8) (a c2)))))

(defun pack-color-* (r g b a)
  (logior (ash (logand (round r) #xFF) 0)
          (ash (logand (round g) #xFF) 8)
          (ash (logand (round b) #xFF) 16)
          (ash (logand (round a) #xFF) 24)))

(defun pack-color (color)
  (pack-color-* (r color) (g color) (b color) (a color)))

(defun unpack-color-* (rgba)
  (declare (type integer rgba))
  (setf rgba (logand rgba #xFFFFFFFF))
  (values (logand (ash rgba -24) #xFF)
          (logand (ash rgba -16) #xFF)
          (logand (ash rgba -8) #xFF)
          (logand (ash rgba -0) #xFF)))

(defun unpack-color (rgba)
  (declare (type integer rgba))
  (multiple-value-bind (r g b a)
      (unpack-color-* rgba)
    (color :r r :g g :b b :a a)))

(defun any-color-but-this (color)
  (color :r (if (>= (r color) #xFF)
		0
		#xFF)))
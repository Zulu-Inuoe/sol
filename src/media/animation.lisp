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

(defclass frame-desc ()
  ((duration
    :type number
    :documentation "Duration of the frame, in seconds"
    :initarg :duration
    :reader duration)
   (path
    :type pathname
    :initarg :path
    :reader path)
   (x
    :type (or number null)
    :initarg :x
    :reader x)
   (y
    :type (or number null)
    :initarg :y
    :reader y)
   (width
    :type (or number null)
    :initarg :width
    :reader width)
   (height
    :type (or number null)
    :initarg :height
    :reader height)
   (flip
    :type (or keyword null)
    :initarg :flip
    :reader flip)))

(defclass lanim ()
  ((name
    :type symbol
    :initform nil :initarg :name
    :reader name)
   (frames
    :type (array frame-desc *) :initarg :frames
    :reader frames)))

(defun load-lanims (path)
  (with-open-file (file path :direction :input :element-type 'character)
    (loop for lanim in (read file)
      collecting
        (make-instance 'lanim
          :name (getf lanim :name)
          :frames (loop for frame in (getf lanim :frames)
                    collecting
                      (make-instance 'frame-desc
                        ;;Duration in file is in ms, convert to seconds
                        :duration (/ (getf frame :duration) 1000)
                        :path (merge-pathnames (getf frame :path) path)
                        :x (getf frame :x)
                        :y (getf frame :y)
                        :width (getf frame :width)
                        :height (getf frame :height)
                        :flip (getf frame :flip)))))))

(defmacro with-lanims ((var path) &body body)
  `(let ((,var (load-lanims (pathname ,path))))
    ,@body))

(defclass frame ()
  ((duration
    :documentation "Duration of the frame, in seconds."
    :type number
    :initarg :duration
    :reader duration)
   (image
    :type image
    :initarg :image
    :reader image)
   (flip
    :type keyword
    :initarg :flip
    :reader flip)))

(defun frame-width (frame)
  (image-width (image frame)))

(defun frame-height (frame)
  (image-height (image frame)))

(defun draw-frame (frame renderer &key (x 0) (y 0))
  (render-draw-image renderer (image frame) x y :flip (flip frame)))

;;;An Animation describes multiple frames ordered in a certain sequence,
;;;and holds the current state of the sequence
(defclass animation ()
  ((name
    :type symbol :reader name :initarg :name)
   (curr-frame
    :type frame :accessor curr-frame)
   (frame-time
    :type number :initform 0)
   (frame-index
    :type integer :initform 0)
   (frames
    :type (array Frame *)
    :reader frames :initarg :frames)))

(defmethod initialize-instance :after ((anim animation) &key)
  (reset-animation anim))

(defun animation-width (anim)
  (frame-width (curr-frame anim)))

(defun animation-height (anim)
  (frame-height (curr-frame anim)))

(defun load-frame (frame-desc)
  (make-instance
   'frame
   :duration (duration frame-desc)
   :image (make-instance
           'image
           :path (path frame-desc)
           :x (x frame-desc)
           :y (y frame-desc)
           :width (width frame-desc)
           :height (height frame-desc))
   :flip (flip frame-desc)))

(defun load-animation (path &optional name)
  (with-lanims (lanims path)
    (let ((lanim (find name lanims :key #'name)))
      (assert (not (null lanim)) ())
      (make-instance 'animation
        :name (name lanim)
        :frames (map 'vector
                     (lambda (frame-desc)
                       (load-frame frame-desc))
                     (frames lanim))))))

(defun load-animations (path)
  (flet ((make-animation (desc)
          (make-instance
           'animation
           :name (name desc)
           :frames (map 'vector
                        (lambda (frame-desc)
                          (load-frame frame-desc))
                        (frames desc)))))
    (with-lanims (lanims path)
      (mapcar #'make-animation lanims))))

(defun draw-animation (anim renderer &key (x 0) (y 0))
  (draw-frame (curr-frame anim) renderer :x x :y y))

(defun reset-animation (anim)
  (with-slots (curr-frame frame-time frame-index frames) anim
    (setf curr-frame (aref frames 0)
          frame-time 0
          frame-index 0)))

(defun update-animation (anim dt)
  (with-slots (curr-frame frame-time frame-index frames) anim
    (incf frame-time dt)

    ;;Keep going through frames until we run out of 'TIME' bahaha
    (loop :while (and (plusp (duration curr-frame))
                      (> frame-time (duration curr-frame)))
       :doing
       (decf frame-time (duration curr-frame))
       (setf frame-index (mod (1+ frame-index) (length frames))
             curr-frame (aref frames frame-index)))))
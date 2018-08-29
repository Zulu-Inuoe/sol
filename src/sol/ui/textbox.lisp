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

(defparameter +disabled-overlay-color+ (media:color :r 0 :g 0 :b 0 :a 128))

(defclass textbox (text-component)
  ((is-enabled
    :type boolean :initform t
    :initarg :is-enabled :accessor is-enabled)
   (e_text-changed
    :type event
    :initform (make-instance 'event :name "text-changed")
    :reader e_text-changed)
   (%cursor-pos
    :type integer
    :initform 0
    :accessor %cursor-pos)
   (%cursor-draw
    :type boolean
    :initform t
    :accessor %cursor-draw)
   (%cursor-blink-rate
    :type real
    :initform 0.530
    :reader %cursor-blink-rate)
   (%time-since-last-blink
    :type real
    :initform 0.0
    :accessor %time-since-last-blink)
   (%prev-mouse-cursor
    :initform nil
    :accessor %prev-mouse-cursor))
  (:default-initargs
   :background media.colors:*white*))

(defmethod initialize-instance :after ((comp textbox) &key &allow-other-keys)
  (setf (%cursor-pos comp) (length (text comp)))

  (add-handler comp 'e_key-down comp '%textbox-on-key-down)
  (add-handler comp 'e_mouse-down comp '%textbox-on-mouse-down)
  (add-handler comp 'e_got-focus comp '%textbox-on-got-focus)
  (add-handler comp 'e_lost-focus comp '%textbox-on-lost-focus)
  (add-handler comp 'e_text-input comp '%textbox-on-text-input)
  (add-handler comp 'e_mouse-enter comp '%textbox-on-mouse-enter)
  (add-handler comp 'e_mouse-leave comp '%textbox-on-mouse-leave))

(defmethod (setf text) (val (comp textbox))
  (when (string-not-equal val (text comp))
    (call-next-method)
    (when (> (%cursor-pos comp) (length (text comp)))
      (setf (%cursor-pos comp) (length (text comp)))
      (setf (%time-since-last-blink comp) 0)
      (setf (%cursor-draw comp) t))
    (event-notify
     (e_text-changed comp)
     val))
  val)

(defmethod measure-override ((comp textbox) available-width available-height)
  (let ((des-w (+ 4 (media:text-width (text-source comp))))
        (des-h (max (media:font-line-height (media:text-font (text-source comp)))
                    (media:text-height (text-source comp)))))
    (when available-width
      (setf des-w (min des-w available-width)))
    (when available-height
      (setf des-h (min des-h available-height)))

    (values des-w des-h)))

(defmethod draw ((comp textbox) renderer)
  (unless (or (zerop (actual-width comp))
              (zerop (actual-height comp)))
    (media:render-draw-text renderer (text-source comp) 2 0)

    (when (and (eq (focused-component (active-focus-manager)) comp)
               (%cursor-draw comp))
      ;;Draw the caret
      (let ((caret-x (+ 2
                        (min
                         (actual-width comp)
                         (media:font-size-text
                          (media:text-font (text-source comp))
                          (subseq (text comp) 0 (%cursor-pos comp))))))
            (caret-y (min
                      (actual-height comp)
                      (media:font-line-height (media:text-font (text-source comp))))))
        (media:render-draw-line renderer caret-x 0 caret-x caret-y)))

    (unless (is-enabled comp)
      (media:render-draw-rect
       renderer
       0 0
       (actual-width comp) (actual-height comp)
       :fill +disabled-overlay-color+))))

(defun %textbox-on-mouse-down (comp args)
  (when (is-enabled comp)
    (setf (handled args) t)
    (setf (focused-component (active-focus-manager)) comp)))

(defun %textbox-on-got-focus (comp args)
  (declare (ignore comp args))
;;  (event-subscribe
;;   (sol:e_time-tick sol:*sol-context*)
;;   comp
;;   '%textbox-on-dt)
  (sdl2:sdl-start-text-input))

(defun %textbox-on-lost-focus (comp args)
  (declare (ignore comp args))
  (sdl2:sdl-stop-text-input)
;;  (event-unsubscribe
;;   (sol:e_time-tick sol:*sol-context*)
;;   comp)
)

(defun %textbox-on-key-down (comp args)
  (case (input:key (args args))
    (:scancode-left
     (setf (%cursor-pos comp) (max 0 (1- (%cursor-pos comp))))
     (setf (%time-since-last-blink comp) 0)
     (setf (%cursor-draw comp) t))
    (:scancode-right
     (setf (%cursor-pos comp) (min (length (text comp)) (1+ (%cursor-pos comp))))
     (setf (%time-since-last-blink comp) 0)
     (setf (%cursor-draw comp) t))
    (:scancode-home
     (setf (%cursor-pos comp) 0)
     (setf (%time-since-last-blink comp) 0)
     (setf (%cursor-draw comp) t))
    (:scancode-end
     (setf (%cursor-pos comp) (length (text comp)))
     (setf (%time-since-last-blink comp) 0)
     (setf (%cursor-draw comp) t))
    (:scancode-backspace
     (cond
       ((zerop (%cursor-pos comp)))
       ((= (%cursor-pos comp) (length (text comp)))
        (decf (%cursor-pos comp))
        (setf (text comp) (subseq (text comp) 0 (%cursor-pos comp))))
       (t
        (decf (%cursor-pos comp))
        (setf (text comp)
              (concatenate 'string
                           (subseq (text comp) 0 (%cursor-pos comp))
                           (subseq (text comp) (1+ (%cursor-pos comp)))))))
     (setf (%time-since-last-blink comp) 0)
     (setf (%cursor-draw comp) t))
    (:scancode-delete
     (cond
       ((= (%cursor-pos comp) (length (text comp))))
       ((zerop (%cursor-pos comp))
        (setf (text comp)
              (concatenate 'string
                           (subseq (text comp) 1))))
       (t
        (setf (text comp)
              (concatenate 'string
                           (subseq (text comp) 0 (%cursor-pos comp))
                           (subseq (text comp) (1+ (%cursor-pos comp)))))))
     (setf (%time-since-last-blink comp) 0)
     (setf (%cursor-draw comp) t))
    (:scancode-v
     (when (input:ctrl (args args))
       (%textbox-input-string-at-cursor comp (sdl2:sdl-get-clipboard-text))))
    (:scancode-c
     (when (input:ctrl (args args))
       (sdl2:sdl-set-clipboard-text "")))))

(defun %textbox-on-text-input (comp args)
  (setf (%time-since-last-blink comp) 0)
  (setf (%cursor-draw comp) t)
  (%textbox-input-string-at-cursor comp (input:text (args args))))

(defun %textbox-on-mouse-enter (comp args)
  (declare (ignore args))
  (setf (%prev-mouse-cursor comp) (input:get-mouse-cursor))
  (input:set-mouse-cursor :ibeam))

(defun %textbox-on-mouse-leave (comp args)
  (declare (ignore args))
  (input:set-mouse-cursor (%prev-mouse-cursor comp))
  (setf (%prev-mouse-cursor comp) nil))

(defun %textbox-input-string-at-cursor (comp text)
  (cond
    ((zerop (%cursor-pos comp))
     (setf (text comp)
           (concatenate 'string text (text comp))))
    ((= (%cursor-pos comp) (length (text comp)))
     (setf (text comp)
           (concatenate 'string (text comp) text)))
    (t
     (setf (text comp)
           (concatenate 'string
                        (subseq (text comp) 0 (%cursor-pos comp))
                        text
                        (subseq (text comp) (%cursor-pos comp))))))
  (incf (%cursor-pos comp) (length text)))

(defun %textbox-on-dt (comp dt)
  (incf (%time-since-last-blink comp) dt)
  (when (>= (%time-since-last-blink comp)
            (%cursor-blink-rate comp))
    (setf (%time-since-last-blink comp) 0)
    (setf (%cursor-draw comp) (not (%cursor-draw comp)))))

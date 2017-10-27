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

(cl:in-package #:sol.ui)

(defparameter +disabled-overlay-color+ (media:color :r 0 :g 0 :b 0 :a 128))

(defclass button (content-control)
  ((command
    :initarg :command
    :accessor command)
   (is-enabled
    :type boolean :initform t
    :initarg :is-enabled :accessor is-enabled)
   (button-state
    :type symbol
    :initform :normal
    :accessor button-state)
   (clicked-color
    :type media:color
    :initarg :clicked-color
    :accessor clicked-color)
   (hover-color
    :type media:color
    :initarg :hover-color
    :accessor hover-color)
   (click-mode
    :type symbol
    :initarg :click-mode
    :accessor click-mode)
   (e_click
    :type routed-event
    :initform (make-instance 'routed-event :name "click")
    :reader e_click))
  (:default-initargs
   :command nil
    :border-thickness 1
    :background  (media:color :a #xff :r #xdd :g #xdd :b #xdd)
    :clicked-color (media:color :a #xff :r #xc4 :g #xe5 :b #xf6)
    :hover-color (media:color :a #xff :r #xbe :g #xe6 :b #xfd)
    :click-mode :release))

(defmethod initialize-instance :after ((comp button) &key)
  (when (command comp)
    (setf (is-enabled comp) (can-execute (command comp) nil))
    (event-subscribe
     (e_can-execute-changed (command comp))
     comp
     (lambda (comp args)
       (declare (ignore args))
       (setf (is-enabled comp) (can-execute (command comp) nil)))))
  ;;Set up the space and enter handler
  (add-handler
   comp
   'e_key-down
   comp
   '%button-on-key-down)

  (add-handler
   comp
   'e_key-up
   comp
   '%button-on-key-up)

  ;;Set up the click handler
  (add-handler
   comp
   'e_mouse-down
   comp
   '%button-on-mouse-down)

  (add-handler
   comp
   'e_mouse-up
   comp
   '%button-on-mouse-up)

  (add-handler
   comp
   'e_lost-focus
   comp
   '%button-on-lost-focus))

(defmethod draw ((comp button) renderer)
  (unless (or (zerop (actual-width comp))
              (zerop (actual-height comp)))

    (let ((fill-color
           (cond
             ((not (is-enabled comp))
              (background comp))
             ((eq (button-state comp) :pressed)
              (clicked-color comp))
             ((is-mouse-over comp)
              (hover-color comp))
             ((eq (button-state comp) :normal)
              (background comp)))))

      (media:render-draw-rect
       renderer
       0 0
       (actual-width comp) (actual-height comp)
       :fill fill-color
       :stroke (border-brush comp)
       ;;TODO Use thickness of each border separately
       :stroke-thickness (left (border-thickness comp))))

    (call-next-method)

    (unless (is-enabled comp)
      (media:render-draw-rect
       renderer
       0 0
       (actual-width comp) (actual-height comp)
       :fill +disabled-overlay-color+))))

(defun %button-on-key-down (comp args)
  (when (and (is-enabled comp)
                (eq (button-state comp) :normal)
                (or (eq (input:key (args args)) :scancode-return)
                    (eq (input:key (args args)) :scancode-space)))
       (setf (handled args) t)
       (setf (button-state comp) :pressed)
       (capture-mouse comp)

       (when (eq (click-mode comp)
                 :press)
         (raise-event
          comp
          'e_click
          (make-instance
           'routed-event-args
           :source comp))
         (when (command comp)
           (execute (command comp) nil)))))

(defun %button-on-key-up (comp args)
  (when (and (is-enabled comp)
                (eq (button-state comp) :pressed)
                (or (eq (input:key (args args)) :scancode-return)
                    (eq (input:key (args args)) :scancode-space)))
    (setf (handled args) t)
    (capture-mouse nil)

    (when (eq (click-mode comp) :release)
      (raise-event
       comp
       'e_click
       (make-instance
        'routed-event-args
        :source comp))
      (when (command comp)
        (execute (command comp) nil)))
    (setf (button-state comp) :normal)))

(defun %button-on-mouse-down (comp args)
  (when (and (is-enabled comp)
             (eq (input:button (args args)) :mouse-button-left)
             (eq (button-state comp) :normal))
    (setf (handled args) t)
    (setf (focused-component (active-focus-manager)) comp)
    (capture-mouse comp)
    (setf (button-state comp) :pressed)

    (when (eq (click-mode comp) :press)
      (raise-event
       comp
       'e_click
       (make-instance
        'routed-event-args
        :source comp))
      (when (command comp)
        (execute (command comp) nil)))))

(defun %button-on-mouse-up (comp args
                            &aux (input-args (args args)))
  (when (and (is-enabled comp)
             (eq (input:button input-args) :mouse-button-left)
             (eq (button-state comp) :pressed))
    (setf (handled args) t)
    (capture-mouse nil)

    (when (and (contains-absolute-*-p
                comp
                (input:x input-args) (input:y input-args))
               (eq (click-mode comp) :release))
      (raise-event
       comp
       'e_click
       (make-instance
        'routed-event-args
        :source comp))
      (when (command comp)
        (execute (command comp) nil)))
    (setf (button-state comp) :normal)))

(defun %button-on-lost-focus (comp args)
  (setf (handled args) t)
  (when (eq (button-state comp) :pressed)
    (setf (button-state comp) :normal)))
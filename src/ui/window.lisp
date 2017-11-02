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

(defclass window (content-control)
  ((focus-manager
    :initform (make-instance 'focus-manager)
    :reader focus-manager)
   (impl
    :reader impl)
   (e_window-closing
    :type event
    :initform (make-instance 'event :name "window-closing")
    :reader e_window-closing)
   (e_window-closed
    :type event
    :initform (make-instance 'event :name "window-closed")
    :reader e_window-closed)
   (%closing
    :type boolean
    :initform nil
    :accessor %closing)
   (%closed
    :type boolean
    :initform nil
    :accessor %closed))
  (:default-initargs
    :horizontal-content-alignment :stretch
    :vertical-content-alignment :stretch))

(defgeneric window-left (comp)
  (:method ((comp window))
    (drivers:window-left (impl comp))))

(defgeneric (setf window-left) (value comp)
  (:method (value (comp window))
    (setf (drivers:window-left (impl comp)) value)))

(defgeneric window-top (comp)
  (:method ((comp window))
    (drivers:window-top (impl comp))))

(defgeneric (setf window-top) (value comp)
  (:method (value (comp window))
    (setf (drivers:window-top (impl comp)) value)))

(defgeneric window-close (comp)
  (:method ((comp window))
    (unless (%closed comp)
      (unless (%closing comp)
        (setf (%closing comp) t)
        (unwind-protect
             (event-notify
              (e_window-closing comp)
              comp)
          (setf (%closing comp) nil))

        (setf (%closed comp) t)

        (unwind-protect
             (dispose (impl comp))
          (slot-makunbound comp 'impl)
          (event-notify
           (e_window-closed comp)
           comp))))
    (values)))

(defmethod initialize-instance :after ((comp window)
                                       &key
                                         (title "")
                                         (left nil)
                                         (top nil)
                                         (width nil)
                                         (height nil)
                                         (state :normal)
                                         (border-style :normal)
                                         (fullscreen nil)
                                         (visible t)
                                       &allow-other-keys)
  (setf (slot-value comp 'impl)
        (make-instance
         (drivers:driver-window-impl (drivers:active-driver))
         :window comp
         :title title
         :x left :y top
         :width width :height height
         :state state
         :border-style border-style
         :fullscreen fullscreen
         :visible visible)))

(declaim (ftype (function () t) current-app))
(defgeneric app-add-window (app window))

(defmethod initialize-instance :around ((comp window) &key &ellow-other-keys)
  (call-next-method)
  (when (current-app)
    (app-add-window (current-app) comp)))

(defmethod (setf width) (val (comp window))
  (setf (drivers:window-width (impl comp)) val)
  (call-next-method (drivers:window-width (impl comp)) comp))

(defmethod (setf height) (val (comp window))
  (setf (drivers::window-height (impl comp)) val)
  (call-next-method (drivers:window-height (impl comp)) comp))

(defmethod measure-override ((comp window) available-width available-height)
  (let ((des-w (drivers:window-width (impl comp)))
        (des-h (drivers:window-height (impl comp))))
    (when (%presenter comp)
      (multiple-value-bind (min-w max-w min-h max-h)
          (%window.min-max comp)
        (let ((w (or (and available-width (max (min available-width max-w) min-w))
                     max-w))
              (h (or (and available-height (max (min available-height max-h) min-h))
                     max-h)))
          (measure (%presenter comp) w h))))
    (values des-w des-h)))

(defmethod arrange-override ((comp window) width height)
  (multiple-value-bind (min-w max-w min-h max-h)
      (%window.min-max comp)
    (setf width (max (min width max-w) min-w))
    (setf height (max (min height max-h) min-h))

    (when (%presenter comp)
      (arrange (%presenter comp) 0 0 width height)))
  (values width height))

(defmethod draw :around ((comp window) renderer)
  (load-component comp)
  (measure comp nil nil)
  (arrange comp 0 0 (desired-width comp) (desired-height comp))
  (call-next-method))

(defun %window.min-max (comp)
  (let (min-w max-w min-h max-h)
    (setf min-w (or (min-width comp) 0))
    (setf min-h (or (min-height comp) 0))
    (setf max-w (or (max-width comp) (drivers:window-width (impl comp))))
    (setf max-h (or (max-height comp) (drivers:window-height (impl comp))))
    (values min-w max-w min-h max-h)))

(defun impl:impl (window)
  (impl window))

(defun impl:impl-closed (window)
  (unless (%closed window)
    (setf (%closed window) t)

    (unwind-protect
         (dispose (impl window))
      (slot-makunbound window 'impl)
      (event-notify
       (e_window-closed window)
       window))))

(defun impl:impl-mouse-button (window args)
  (when-let ((target (or (capturing-component)
                         (get-component-at-* window (input:x args) (input:y args)))))
    (raise-event
     target
     (if (input:button-pressed args)
         'e_mouse-down
         'e_mouse-up)
     (make-instance
      'routed-input-args
      :source target
      :args args))))

(defun impl:impl-mouse-move (window args)
  (when-let ((target (or (capturing-component)
                         (get-component-at-* window (input:x args) (input:y args)))))
    (raise-event
     target
     'e_mouse-move
     (make-instance
      'routed-input-args
      :source target
      :args args))

    (unless (capturing-component)
      (setf (mouse-over-component) target))))

(defun impl:impl-mouse-wheel (window args)
  (when-let ((target (or (capturing-component)
                         (get-component-at-* window (input:x args) (input:y args)))))
    (raise-event
     target
     'e_mouse-wheel
     (make-instance
      'routed-input-args
      :source target
      :args args))))

(defun impl:impl-key (window args)
  (when-let ((target (focused-component (active-focus-manager))))
    (raise-event
     target
     (if (input:key-pressed args)
         'e_key-down
         'e_key-up)
     (make-instance
      'routed-input-args
      :source target
      :args args))))

(defun impl:impl-text-input (window args)
  (when-let ((target (focused-component (active-focus-manager))))
    (raise-event
     target
     'e_text-input
     (make-instance
      'routed-input-args
      :source target
      :args args))))
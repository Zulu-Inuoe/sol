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

(defclass sdl2-window-impl (finalizable)
  ((ui-window
    :type ui:window
    :initarg :window
    :initform (error "sdl2-window-impl: must supply window")
    :reader ui-window)
   (sdl-window
    :type sdl2-ffi:sdl-window
    :reader sdl-window)
   (renderer
    :reader renderer)
   (sdl-window-l
    :type integer
    :accessor sdl-window-l)
   (sdl-window-t
    :type integer
    :accessor sdl-window-t)
   (sdl-window-w
    :type integer
    :accessor sdl-window-w)
   (sdl-window-h
    :type integer
    :accessor sdl-window-h)))

(defgeneric sdl-create-renderer (impl sdl-window))
(defgeneric sdl-window-event (impl event-type data1 data2))
(defgeneric sdl-draw (impl))

(defmethod sdl-create-renderer ((impl sdl2-window-impl) sdl-window)
  (let ((sdl-renderer
         (sdl2:create-renderer
          sdl-window -1 (list :accelerated))))
    (sdl2:set-render-draw-blend-mode sdl-renderer sdl2-ffi:+sdl-blendmode-blend+)
    (make-instance 'sdl2-sdl-renderer :native sdl-renderer)))

(defvar *%in-resize* nil)
(defvar *%in-closing* nil)
(defmethod sdl-window-event ((impl sdl2-window-impl) event-type data1 data2)
  (case event-type
    (:shown
     (sdl-draw impl))
    (:hidden)
    (:exposed
     (sdl-draw impl))
    (:moved
     (setf (sdl-window-l impl) data1
           (sdl-window-t impl) data2))
    (:resized
     (setf (sdl-window-w impl) data1
           (sdl-window-h impl) data2)
     (let ((*%in-resize* t))
       (setf (ui:width (ui-window impl)) (sdl-window-w impl)
             (ui:height (ui-window impl)) (sdl-window-h impl)))
     (sdl-draw impl))
    (:size-changed
     (setf (sdl-window-w impl) data1
           (sdl-window-h impl) data2)
     (let ((*%in-resize* t))
       (setf (ui:width (ui-window impl)) (sdl-window-w impl)
             (ui:height (ui-window impl)) (sdl-window-h impl)))
     (sdl-draw impl))
    (:minimized)
    (:maximized)
    (:restored)
    (:enter
     (multiple-value-bind (x y) (input:mouse-position)
       (setf (ui:mouse-over-component)
             (ui:get-component-at-* (ui-window impl) (- x (window-left impl)) (- y (window-top impl))))))
    (:leave
     (setf (ui:mouse-over-component) nil))
    (:focus-gained
     (setf (ui:active-focus-manager) (ui:focus-manager (ui-window impl))))
    (:focus-lost
     (setf (ui:active-focus-manager) nil))
    (:close
     (let ((*%in-closing* t))
       (ui.impl:impl-closed (ui-window impl))
       (dispose impl)))))

(defmethod sdl-draw ((impl sdl2-window-impl) &aux (renderer (renderer impl)))
  (media:render-clear renderer :color media.colors:*white*)
  (ui:draw (ui-window impl) renderer)
  (media:render-present renderer))

#+(and win32 swank)
(defvar *%sdl-window-first-window* t)

(defmethod initialize-instance :after ((impl sdl2-window-impl)
                                       &key
                                         (title "")
                                         (x nil)
                                         (y nil)
                                         (width 800)
                                         (height 600)
                                         (state :normal)
                                         (border-style :normal)
                                         (fullscreen nil)
                                         (visible t)
                                         &allow-other-keys)
    (let* ((sdl-window
            (sdl2-ffi.functions:sdl-create-window
             title
             (or (and x (round x)) sdl2-ffi:+sdl-windowpos-undefined+)
             (or (and y (round y)) sdl2-ffi:+sdl-windowpos-undefined+)
             (or (and width (round width)) 800)
             (or (and height (round height)) 600)
             (logior
              (if visible sdl2-ffi:+sdl-window-shown+ sdl2-ffi:+sdl-window-hidden+)
              sdl2-ffi:+sdl-window-resizable+
              sdl2-ffi:+sdl-window-opengl+
              (ecase state
                ((nil :normal) 0)
                (:minimized sdl2-ffi:+sdl-window-minimized+)
                (:maximized sdl2-ffi:+sdl-window-maximized+))
              (ecase border-style
                ((nil :normal) 0)
                (:borderless sdl2-ffi:+sdl-window-borderless+))
              (if fullscreen
                  sdl2-ffi:+sdl-window-fullscreen+
                  0))))
           (renderer (sdl-create-renderer impl sdl-window)))

      (cffi:with-foreign-objects ((x :int) (y :int))
        (sdl2-ffi.functions:sdl-get-window-position sdl-window x y)
        (setf (sdl-window-l impl) (cffi:mem-ref x :int)
              (sdl-window-t impl) (cffi:mem-ref y :int)))

      (cffi:with-foreign-objects ((width :int) (height :int))
        (sdl2-ffi.functions:sdl-gl-get-drawable-size sdl-window width height)
        (setf (sdl-window-w impl) (cffi:mem-ref width :int)
              (sdl-window-h impl) (cffi:mem-ref height :int)))

      (setf (slot-value impl 'sdl-window) sdl-window
            (slot-value impl 'renderer) renderer)

      #+(and win32 swank)
      (progn
        (when (and visible *%sdl-window-first-window*)
          (sdl2:hide-window sdl-window)
          (sdl2:show-window sdl-window)
          (sdl2:hide-window sdl-window)
          (sdl2:show-window sdl-window))
        (setf *%sdl-window-first-window* nil)))

    (event-subscribe
     *e_sdl2-event*
     impl
     '%sdl2-window-impl.sdl2-event))

(define-finalizer sdl2-window-impl (sdl-window renderer)
  (unwind-protect
       (dispose renderer)
    (sdl2-ffi.functions:sdl-destroy-window sdl-window)))

(defmethod dispose ((impl sdl2-window-impl))
  (event-unsubscribe
   *e_sdl2-event*
   impl)
  (unless *%in-closing*
    (ui.impl:impl-closed (ui-window impl)))
  (slot-makunbound impl 'ui-window)
  (call-next-method)
  (slot-makunbound impl 'renderer)
  (slot-makunbound impl 'sdl-window))

(defmethod window-left ((impl sdl2-window-impl))
  (sdl-window-l impl))

(defmethod (setf window-left) (value (impl sdl2-window-impl))
  (setf value (round value))
  (sdl2-ffi.functions:sdl-set-window-position
   (sdl-window impl)
   value
   sdl2-ffi:+sdl-windowpos-undefined+)
  (%sdl2-window-impl.refresh-pos impl)
  (setf value (sdl-window-l impl))
  value)

(defmethod window-top ((impl sdl2-window-impl))
  (sdl-window-t impl))

(defmethod (setf window-top) (value (impl sdl2-window-impl))
  (setf value (round value))
  (sdl2-ffi.functions:sdl-set-window-position
   (sdl-window impl)
   sdl2-ffi:+sdl-windowpos-undefined+
   value)
  (%sdl2-window-impl.refresh-pos impl)
  (setf value (sdl-window-t impl))
  value)

(defmethod window-width ((impl sdl2-window-impl))
  (sdl-window-w impl))

(defmethod (setf window-width) (value (impl sdl2-window-impl))
  (unless *%in-resize*
    (setf value (round value))
    (sdl2-ffi.functions:sdl-set-window-size
     (sdl-window impl)
     value
     (sdl-window-h impl))
    (%sdl2-window-impl.refresh-size impl))
  (sdl-window-h impl))

(defmethod window-height ((impl sdl2-window-impl))
  (sdl-window-h impl))

(defmethod (setf window-height) (value (impl sdl2-window-impl))
  (unless *%in-resize*
    (setf value (round value))
    (sdl2-ffi.functions:sdl-set-window-size
     (sdl-window impl)
     (sdl-window-w impl)
     value)
    (%sdl2-window-impl.refresh-size impl))
  (sdl-window-h impl))

(defun %sdl2-window-impl.refresh-pos (impl
                                       &aux (sdl-window (sdl-window impl)))
  (cffi:with-foreign-objects ((x :int) (y :int))
        (sdl2-ffi.functions:sdl-get-window-position sdl-window x y)
        (setf (sdl-window-l impl) (cffi:mem-ref x :int)
              (sdl-window-t impl) (cffi:mem-ref y :int)))
  (values))

(defun %sdl2-window-impl.refresh-size (impl
                                       &aux (sdl-window (sdl-window impl)))
  (cffi:with-foreign-objects ((width :int) (height :int))
    (sdl2-ffi.functions:sdl-gl-get-drawable-size sdl-window width height)
    (setf (sdl-window-w impl) (cffi:mem-ref width :int)
          (sdl-window-h impl) (cffi:mem-ref height :int)))
  (values))

(defun %sdl2-window-impl.sdl2-event (impl sdl-event
                                     &aux
                                       (ui-window (ui-window impl))
                                       (sdl-event-type (sdl2:get-event-type sdl-event)))
  (unless (%window-owns-event-p (sdl-window impl) sdl-event)
    (return-from %sdl2-window-impl.sdl2-event))

  (case sdl-event-type
    ((:mousebuttondown :mousebuttonup)
     (let ((device :keyboard/mouse)
           (mod (sdl2-ffi.functions:sdl-get-mod-state))
           (button
            (%mouse-val->sym
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :button :button)))
           (button-pressed
            (= (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :button :state)
               sdl2-ffi:+sdl-pressed+)))
       (multiple-value-bind (state x y)
           (%mouse-state)
         (ui.impl:impl-mouse-button
          ui-window
          (make-instance
           'input:mouse-button-event-args
           :device device
           :alt (logtest mod sdl2-ffi:+kmod-alt+)
           :ctrl (logtest mod sdl2-ffi:+kmod-ctrl+)
           :shift (logtest mod sdl2-ffi:+kmod-shift+)
           :gui (logtest mod sdl2-ffi:+kmod-gui+)
           :x x
           :y y
           :left-button (logtest state sdl2-ffi:+sdl-button-lmask+)
           :middle-button (logtest state sdl2-ffi:+sdl-button-mmask+)
           :right-button (logtest state sdl2-ffi:+sdl-button-rmask+)
           :x1-button (logtest state sdl2-ffi:+sdl-button-x1mask+)
           :x2-button (logtest state sdl2-ffi:+sdl-button-x2mask+)
           :button button
           :button-pressed button-pressed)))))
    (:mousemotion
     (let ((device
            :keyboard/mouse)
           (mod (sdl2-ffi.functions:sdl-get-mod-state)))
       (multiple-value-bind (state x y)
           (%mouse-state)
         (ui.impl:impl-mouse-move
          ui-window
          (make-instance
           'input:mouse-event-args
           :device device
           :alt (logtest mod sdl2-ffi:+kmod-alt+)
           :ctrl (logtest mod sdl2-ffi:+kmod-ctrl+)
           :shift (logtest mod sdl2-ffi:+kmod-shift+)
           :gui (logtest mod sdl2-ffi:+kmod-gui+)
           :x x
           :y y
           :left-button (logtest state sdl2-ffi:+sdl-button-lmask+)
           :middle-button (logtest state sdl2-ffi:+sdl-button-mmask+)
           :right-button (logtest state sdl2-ffi:+sdl-button-rmask+)
           :x1-button (logtest state sdl2-ffi:+sdl-button-x1mask+)
           :x2-button (logtest state sdl2-ffi:+sdl-button-x2mask+))))))
    (:mousewheel
     (let ((device
            :keyboard/mouse)
           (mod (sdl2-ffi.functions:sdl-get-mod-state))
           (delta (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :wheel :y))
           (direction (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :wheel :direction)))
       (when (= direction sdl2-ffi:+sdl-mousewheel-flipped+)
         (setf direction (- direction)))

       (multiple-value-bind (state x y)
           (%mouse-state)
         (ui.impl:impl-mouse-wheel
          ui-window
          (make-instance
           'input:mouse-wheel-event-args
           :device device
           :alt (logtest mod sdl2-ffi:+kmod-alt+)
           :ctrl (logtest mod sdl2-ffi:+kmod-ctrl+)
           :shift (logtest mod sdl2-ffi:+kmod-shift+)
           :gui (logtest mod sdl2-ffi:+kmod-gui+)
           :x x
           :y y
           :left-button (logtest state sdl2-ffi:+sdl-button-lmask+)
           :middle-button (logtest state sdl2-ffi:+sdl-button-mmask+)
           :right-button (logtest state sdl2-ffi:+sdl-button-rmask+)
           :x1-button (logtest state sdl2-ffi:+sdl-button-x1mask+)
           :x2-button (logtest state sdl2-ffi:+sdl-button-x2mask+)
           :delta delta)))))
    (:windowevent
     (let* ((event-type
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :window :event))
            (data1
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :window :data1))
            (data2
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :window :data2)))
       (sdl-window-event impl (%window-event->sym event-type) data1 data2)))
    ((:keydown :keyup)
     (let* ((device
             :keyboard/mouse)
            (keysym
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event :key :keysym))
            (mod (sdl2:mod-value keysym))
            (key (sdl2:scancode-symbol
                  (sdl2:scancode-value keysym)))
            (key-pressed
             (= (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :key :state)
                sdl2-ffi:+sdl-pressed+))
            (repeat
             (/= 0  (plus-c:c-ref
                     sdl-event
                     sdl2-ffi:sdl-event
                     :key :repeat))))
       (declare (ignore repeat))
       (ui.impl:impl-key
        ui-window
        (make-instance
         'input:key-event-args
         :device device
         :alt (logtest mod sdl2-ffi:+kmod-alt+)
         :ctrl (logtest mod sdl2-ffi:+kmod-ctrl+)
         :shift (logtest mod sdl2-ffi:+kmod-shift+)
         :gui (logtest mod sdl2-ffi:+kmod-gui+)
         :key key
         :pressed key-pressed))))
    (:textinput
     (let* ((text-ptr
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :text :text plus-c:&))
            (text
             (cffi:foreign-string-to-lisp
              text-ptr
              :max-chars 31
              :encoding :utf-8)))
       (ui.impl:impl-text-input
        ui-window
        (make-instance
         'input:text-input-event-args
         :text text))))
    (:textediting)))

(defun %window-owns-event-p (sdl-window sdl-event
                             &aux
                               (window-id (sdl2:get-window-id sdl-window))
                               (sdl-event-type (sdl2:get-event-type sdl-event)))
  (eql
   window-id
   (case sdl-event-type
     ((:mousebuttondown :mousebuttonup)
      (plus-c:c-ref
       sdl-event
       sdl2-ffi:sdl-event
       :button :window-id))
     (:mousemotion
      (plus-c:c-ref
       sdl-event
       sdl2-ffi:sdl-event
       :motion :window-id))
     (:mousewheel
      (plus-c:c-ref
       sdl-event
       sdl2-ffi:sdl-event
       :wheel :window-id))
     (:windowevent
      (plus-c:c-ref
       sdl-event
       sdl2-ffi:sdl-event
       :window :window-id))
     ((:keydown :keyup)
      (plus-c:c-ref
       sdl-event
       sdl2-ffi:sdl-event
       :key :window-id))
     (:textinput
      (plus-c:c-ref
       sdl-event
       sdl2-ffi:sdl-event
       :text :window-id))
     (:textediting
      (plus-c:c-ref
       sdl-event
       sdl2-ffi:sdl-event
       :edit :window-id))
     (t nil))))

(defun %window-event->sym (val)
  (eswitch (val)
    (sdl2-ffi:+sdl-windowevent-none+ :none)
    (sdl2-ffi:+sdl-windowevent-shown+ :shown)
    (sdl2-ffi:+sdl-windowevent-hidden+ :hidden)
    (sdl2-ffi:+sdl-windowevent-exposed+ :exposed)
    (sdl2-ffi:+sdl-windowevent-moved+ :moved)
    (sdl2-ffi:+sdl-windowevent-resized+ :resized)
    (sdl2-ffi:+sdl-windowevent-size-changed+ :size-changed)
    (sdl2-ffi:+sdl-windowevent-minimized+ :minimized)
    (sdl2-ffi:+sdl-windowevent-maximized+ :maximized)
    (sdl2-ffi:+sdl-windowevent-restored+ :restored)
    (sdl2-ffi:+sdl-windowevent-enter+ :enter)
    (sdl2-ffi:+sdl-windowevent-leave+ :leave)
    (sdl2-ffi:+sdl-windowevent-focus-gained+ :focus-gained)
    (sdl2-ffi:+sdl-windowevent-focus-lost+ :focus-lost)
    (sdl2-ffi:+sdl-windowevent-close+ :close)))

(defun %mouse-val->sym (val)
  (eswitch (val)
    (sdl2-ffi:+sdl-button-left+
     :mouse-button-left)
    (sdl2-ffi:+sdl-button-right+
     :mouse-button-right)
    (sdl2-ffi:+sdl-button-middle+
     :mouse-button-middle)
    (sdl2-ffi:+sdl-button-x1+
     :mouse-button-x1)
    (sdl2-ffi:+sdl-button-x2+
     :mouse-button-x2)))

(defun %mouse-state ()
  (cffi:with-foreign-objects ((x :int) (y :int))
    (let ((state (sdl2-ffi.functions:sdl-get-mouse-state x y)))
      (values state (cffi:mem-ref x :int) (cffi:mem-ref y :int)))))
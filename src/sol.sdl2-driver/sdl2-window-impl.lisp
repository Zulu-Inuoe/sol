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

(defclass sdl2-window-impl ()
  ((ui-window
    :type ui:window
    :initarg :window
    :initform (error "sdl2-window-impl: must supply window")
    :reader ui-window)
   (sdl-window
    :type sdl2:sdl-window
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
    :accessor sdl-window-h)
   (closed
    :type boolean
    :initform nil
    :accessor closed)))

(defgeneric sdl-create-renderer (impl sdl-window))
(defgeneric sdl-window-event (impl event-type data1 data2))
(defgeneric sdl-draw (impl)
  (:documentation "Called when a rendering pass is requested"))

(defmethod sdl-create-renderer ((impl sdl2-window-impl) sdl-window)
  (let ((sdl-renderer
         (sdl2:sdl-create-renderer
          sdl-window -1 sdl2:+sdl-renderer-accelerated+)))
    (sdl2:sdl-set-render-draw-blend-mode sdl-renderer sdl2:+sdl-blendmode-blend+)
    (make-instance 'sdl2-sdl-renderer :native sdl-renderer)))

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
    ((:resized :size-changed)
     (unless (and (= (sdl-window-w impl) data1)
                  (= (sdl-window-h impl) data2))
       (setf (sdl-window-w impl) data1
             (sdl-window-h impl) data2)
       (ui.impl:impl-resized (ui-window impl)))
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
     (setf (closed impl) t)
     (dispatcher:do-begin-invoke ((dispatcher:dispatcher (ui-window impl)))
       (window-close impl)
       (ui.impl:impl-closed (ui-window impl))))))

(defmethod sdl-draw :around ((impl sdl2-window-impl) &aux (renderer (renderer impl)))
  (media:render-clear renderer :color media.colors:*white*)
  (call-next-method)
  (media:render-present renderer))

(defmethod sdl-draw ((impl sdl2-window-impl) &aux (renderer (renderer impl)))
  (ui:draw (ui-window impl) renderer))

#+(and os-windows (or slynk swank))
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
  ;;Normalize some values
  (setf x (or (and x (round x)) sdl2:+sdl-windowpos-undefined+))
  (setf y (or (and y (round y)) sdl2:+sdl-windowpos-undefined+))
  (setf width (or (and width (round width)) 800))
  (setf height (or (and height (round height)) 600))

  #+nil
  (progn
    (unless (= x sdl2:+sdl-windowpos-undefined+)
      (incf x (win32:get-system-metrics win32:+sm-cxsizeframe+)))
    (unless (= y sdl2:+sdl-windowpos-undefined+)
      (incf y (+ (win32:get-system-metrics win32:+sm-cycaption+)
                 (win32:get-system-metrics win32:+sm-cysizeframe+))))
    (decf width (* 2 (win32:get-system-metrics win32:+sm-cxsizeframe+)))
    (decf height (+ (win32:get-system-metrics win32:+sm-cycaption+)
                    (* 2 (win32:get-system-metrics win32:+sm-cysizeframe+)))))
  (let* ((sdl-window
          (sdl2:sdl-create-window
           title
           x y
           width height
           (logior
            (if visible sdl2:+sdl-window-shown+ sdl2:+sdl-window-hidden+)
            sdl2:+sdl-window-resizable+
            sdl2:+sdl-window-opengl+
            (ecase state
              ((nil :normal) 0)
              (:minimized sdl2:+sdl-window-minimized+)
              (:maximized sdl2:+sdl-window-maximized+))
            (ecase border-style
              ((nil :normal) 0)
              (:borderless sdl2:+sdl-window-borderless+))
            (if fullscreen
                sdl2:+sdl-window-fullscreen+
                0))))
         (renderer (sdl-create-renderer impl sdl-window)))
    (setf (slot-value impl 'sdl-window) sdl-window
          (slot-value impl 'renderer) renderer)

    (setf (gethash (sdl2:sdl-get-window-id sdl-window) %*sdl2-windows*) (tg:make-weak-pointer impl))

    (%sdl2-window-impl.refresh-pos impl)
    (%sdl2-window-impl.refresh-size impl)

    #+(and os-windows (or slynk swank))
    (progn
      (when (and visible *%sdl-window-first-window*)
        (sdl2:sdl-hide-window sdl-window)
        (sdl2:sdl-show-window sdl-window)
        (sdl2:sdl-hide-window sdl-window)
        (sdl2:sdl-show-window sdl-window))
      (setf *%sdl-window-first-window* nil))))

(defmethod window-close ((impl sdl2-window-impl))
  (setf (closed impl) t)
  (remhash (sdl2:sdl-get-window-id (sdl-window impl)) %*sdl2-windows*)
  (dispose (renderer impl))
  (slot-makunbound impl 'renderer)
  (sdl2:sdl-destroy-window (sdl-window impl))
  (slot-makunbound impl 'sdl-window))

(defmethod window-left ((impl sdl2-window-impl))
  (sdl-window-l impl))

(defmethod (setf window-left) (value (impl sdl2-window-impl))
  (setf value (round value))
  (sdl2:sdl-set-window-position
   (sdl-window impl)
   value
   sdl2:+sdl-windowpos-undefined+)
  (%sdl2-window-impl.refresh-pos impl)
  (setf value (sdl-window-l impl))
  value)

(defmethod window-top ((impl sdl2-window-impl))
  (sdl-window-t impl))

(defmethod (setf window-top) (value (impl sdl2-window-impl))
  (setf value (round value))
  (sdl2:sdl-set-window-position
   (sdl-window impl)
   sdl2:+sdl-windowpos-undefined+
   value)
  (%sdl2-window-impl.refresh-pos impl)
  (setf value (sdl-window-t impl))
  value)

(defmethod window-width ((impl sdl2-window-impl))
  #-fake-fake
  (sdl-window-w impl)
  #+fake-fake
  (+ (sdl-window-w impl)
     (* 2 (win32:get-system-metrics win32:+sm-cxsizeframe+))))

(defmethod (setf window-width) (value (impl sdl2-window-impl))
  (setf value (round value))
  (unless (= value (window-width impl))
    #+fake-fake
    (decf value (* 2 (win32:get-system-metrics win32:+sm-cxsizeframe+)))

    (sdl2:sdl-set-window-size
     (sdl-window impl)
     value
     (sdl-window-h impl))
    (%sdl2-window-impl.refresh-size impl))
  (window-width impl))

(defmethod window-height ((impl sdl2-window-impl))
  #-fake-fake
  (sdl-window-h impl)
  #+fake-fake
  (+ (sdl-window-h impl)
     (win32:get-system-metrics win32:+sm-cycaption+)
     (* 2 (win32:get-system-metrics win32:+sm-cysizeframe+))))

(defmethod (setf window-height) (value (impl sdl2-window-impl))
  (setf value (round value))
  (unless (= value (window-height impl))
    #+fake-fake
    (decf value (+ (win32:get-system-metrics win32:+sm-cycaption+)
                   (* 2 (win32:get-system-metrics win32:+sm-cysizeframe+))))
    (sdl2:sdl-set-window-size
     (sdl-window impl)
     (sdl-window-w impl)
     value)
    (%sdl2-window-impl.refresh-size impl))
  (window-height impl))

(defmethod window-draw-width ((impl sdl2-window-impl))
  (sdl-window-w impl))

(defmethod window-draw-height ((impl sdl2-window-impl))
  (sdl-window-h impl))

(defun %sdl2-window-impl.refresh-pos (impl
                                       &aux (sdl-window (sdl-window impl)))
  (cffi:with-foreign-objects ((x :int) (y :int))
     (sdl2:sdl-get-window-position sdl-window x y)
     (setf (sdl-window-l impl) (cffi:mem-ref x :int)
           (sdl-window-t impl) (cffi:mem-ref y :int)))
  (values))

(defun %sdl2-window-impl.refresh-size (impl
                                       &aux (sdl-window (sdl-window impl)))
  (cffi:with-foreign-objects ((width :int) (height :int))
    (sdl2:sdl-gl-get-drawable-size sdl-window width height)
    (setf (sdl-window-w impl) (cffi:mem-ref width :int)
          (sdl-window-h impl) (cffi:mem-ref height :int)))
  (values))

(defun %sdl2-window-impl.sdl2-event (impl sdl-event
                                     &aux
                                       (ui-window (ui-window impl))
                                       (sdl-event-type
                                        (cffi:foreign-slot-value sdl-event 'sdl2:sdl-event 'sdl2:type)))
  (case sdl-event-type
    ((#.sdl2:+sdl-mousebuttondown+ #.sdl2:+sdl-mousebuttonup+)
     (let ((device :keyboard/mouse)
           (mod (sdl2:sdl-get-mod-state))
           (button
             (%mouse-val->sym
              (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-mouse-button-event) 'sdl2:button)))
           (button-pressed
             (=
              (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-mouse-button-event) 'sdl2:state)
              sdl2:+sdl-pressed+)))
       (multiple-value-bind (state x y)
           (%mouse-state)
         (ui.impl:impl-mouse-button
          ui-window
          (make-instance
           'input:mouse-button-event-args
           :device device
           :alt (logtest mod sdl2:+kmod-alt+)
           :ctrl (logtest mod sdl2:+kmod-ctrl+)
           :shift (logtest mod sdl2:+kmod-shift+)
           :gui (logtest mod sdl2:+kmod-gui+)
           :x x
           :y y
           :left-button (logtest state sdl2:+sdl-button-lmask+)
           :middle-button (logtest state sdl2:+sdl-button-mmask+)
           :right-button (logtest state sdl2:+sdl-button-rmask+)
           :x1-button (logtest state sdl2:+sdl-button-x1mask+)
           :x2-button (logtest state sdl2:+sdl-button-x2mask+)
           :button button
           :button-pressed button-pressed)))))
    (#.sdl2:+sdl-mousemotion+
     (let ((device
            :keyboard/mouse)
           (mod (sdl2:sdl-get-mod-state)))
       (multiple-value-bind (state x y)
           (%mouse-state)
         (ui.impl:impl-mouse-move
          ui-window
          (make-instance
           'input:mouse-event-args
           :device device
           :alt (logtest mod sdl2:+kmod-alt+)
           :ctrl (logtest mod sdl2:+kmod-ctrl+)
           :shift (logtest mod sdl2:+kmod-shift+)
           :gui (logtest mod sdl2:+kmod-gui+)
           :x x
           :y y
           :left-button (logtest state sdl2:+sdl-button-lmask+)
           :middle-button (logtest state sdl2:+sdl-button-mmask+)
           :right-button (logtest state sdl2:+sdl-button-rmask+)
           :x1-button (logtest state sdl2:+sdl-button-x1mask+)
           :x2-button (logtest state sdl2:+sdl-button-x2mask+))))))
    (#.sdl2:+sdl-mousewheel+
     (let ((device
            :keyboard/mouse)
           (mod (sdl2:sdl-get-mod-state))
           (delta (cffi:foreign-slot-value sdl-event '(:struct  sdl2:sdl-mouse-wheel-event) 'sdl2:y))
           (direction (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-mouse-wheel-event) 'sdl2:direction)))
       (when (= direction sdl2:+sdl-mousewheel-flipped+)
         (setf direction (- direction)))

       (multiple-value-bind (state x y)
           (%mouse-state)
         (ui.impl:impl-mouse-wheel
          ui-window
          (make-instance
           'input:mouse-wheel-event-args
           :device device
           :alt (logtest mod sdl2:+kmod-alt+)
           :ctrl (logtest mod sdl2:+kmod-ctrl+)
           :shift (logtest mod sdl2:+kmod-shift+)
           :gui (logtest mod sdl2:+kmod-gui+)
           :x x
           :y y
           :left-button (logtest state sdl2:+sdl-button-lmask+)
           :middle-button (logtest state sdl2:+sdl-button-mmask+)
           :right-button (logtest state sdl2:+sdl-button-rmask+)
           :x1-button (logtest state sdl2:+sdl-button-x1mask+)
           :x2-button (logtest state sdl2:+sdl-button-x2mask+)
           :delta delta)))))
    (#.sdl2:+sdl-window-event+
     (let* ((event-type
              (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-window-event) 'sdl2:event))
            (data1
              (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-window-event) 'sdl2:data1))
            (data2
              (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-window-event) 'sdl2:data2)))
       (unless (closed impl)
         (sdl-window-event impl (%window-event->sym event-type) data1 data2))))
    ((#.sdl2:+sdl-keydown+ #.sdl2:+sdl-keyup+)
     (let* ((device
             :keyboard/mouse)
            (keysym
              (cffi:foreign-slot-pointer sdl-event '(:struct sdl2:sdl-keyboard-event) 'sdl2:keysym))
            (mod (cffi:foreign-slot-value keysym '(:struct sdl2:sdl-keysym) 'sdl2:mod))
            (key (%scancode->sym
                  (cffi:foreign-slot-value keysym '(:struct sdl2:sdl-keysym) 'sdl2:scancode)))
            (key-pressed
              (= (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-keyboard-event) 'sdl2:state)
                sdl2:+sdl-pressed+))
            (repeat
              (/= 0 (cffi:foreign-slot-value sdl-event '(:struct sdl2:sdl-keyboard-event) 'sdl2:repeat))))
       (declare (ignore repeat))
       (ui.impl:impl-key
        ui-window
        (make-instance
         'input:key-event-args
         :device device
         :alt (logtest mod sdl2:+kmod-alt+)
         :ctrl (logtest mod sdl2:+kmod-ctrl+)
         :shift (logtest mod sdl2:+kmod-shift+)
         :gui (logtest mod sdl2:+kmod-gui+)
         :key key
         :pressed key-pressed))))
    (#.sdl2:+sdl-textinput+
     (let* ((text-ptr
              (cffi:foreign-slot-pointer sdl-event '(:struct sdl2:sdl-text-input-event) 'sdl2:text))
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
    (#.sdl2:+sdl-textediting+)))

(defun %window-event->sym (val)
  (ecase val
    (#.sdl2:+sdl-windowevent-none+ :none)
    (#.sdl2:+sdl-windowevent-shown+ :shown)
    (#.sdl2:+sdl-windowevent-hidden+ :hidden)
    (#.sdl2:+sdl-windowevent-exposed+ :exposed)
    (#.sdl2:+sdl-windowevent-moved+ :moved)
    (#.sdl2:+sdl-windowevent-resized+ :resized)
    (#.sdl2:+sdl-windowevent-size-changed+ :size-changed)
    (#.sdl2:+sdl-windowevent-minimized+ :minimized)
    (#.sdl2:+sdl-windowevent-maximized+ :maximized)
    (#.sdl2:+sdl-windowevent-restored+ :restored)
    (#.sdl2:+sdl-windowevent-enter+ :enter)
    (#.sdl2:+sdl-windowevent-leave+ :leave)
    (#.sdl2:+sdl-windowevent-focus-gained+ :focus-gained)
    (#.sdl2:+sdl-windowevent-focus-lost+ :focus-lost)
    (#.sdl2:+sdl-windowevent-close+ :close)
    (#.sdl2:+sdl-windowevent-take-focus+ :take-focus)
    (#.sdl2:+sdl-windowevent-hit-test+ :hit-test)))

(defun %scancode->sym (val)
  (ecase val
    (#.sdl2:+sdl-scancode-unknown+ :scancode-unknown)
    (#.sdl2:+sdl-scancode-a+ :scancode-a)
    (#.sdl2:+sdl-scancode-b+ :scancode-b)
    (#.sdl2:+sdl-scancode-c+ :scancode-c)
    (#.sdl2:+sdl-scancode-d+ :scancode-d)
    (#.sdl2:+sdl-scancode-e+ :scancode-e)
    (#.sdl2:+sdl-scancode-f+ :scancode-f)
    (#.sdl2:+sdl-scancode-g+ :scancode-g)
    (#.sdl2:+sdl-scancode-h+ :scancode-h)
    (#.sdl2:+sdl-scancode-i+ :scancode-i)
    (#.sdl2:+sdl-scancode-j+ :scancode-j)
    (#.sdl2:+sdl-scancode-k+ :scancode-k)
    (#.sdl2:+sdl-scancode-l+ :scancode-l)
    (#.sdl2:+sdl-scancode-m+ :scancode-m)
    (#.sdl2:+sdl-scancode-n+ :scancode-n)
    (#.sdl2:+sdl-scancode-o+ :scancode-o)
    (#.sdl2:+sdl-scancode-p+ :scancode-p)
    (#.sdl2:+sdl-scancode-q+ :scancode-q)
    (#.sdl2:+sdl-scancode-r+ :scancode-r)
    (#.sdl2:+sdl-scancode-s+ :scancode-s)
    (#.sdl2:+sdl-scancode-t+ :scancode-t)
    (#.sdl2:+sdl-scancode-u+ :scancode-u)
    (#.sdl2:+sdl-scancode-v+ :scancode-v)
    (#.sdl2:+sdl-scancode-w+ :scancode-w)
    (#.sdl2:+sdl-scancode-x+ :scancode-x)
    (#.sdl2:+sdl-scancode-y+ :scancode-y)
    (#.sdl2:+sdl-scancode-z+ :scancode-z)

    (#.sdl2:+sdl-scancode-1+ :scancode-1)
    (#.sdl2:+sdl-scancode-2+ :scancode-2)
    (#.sdl2:+sdl-scancode-3+ :scancode-3)
    (#.sdl2:+sdl-scancode-4+ :scancode-4)
    (#.sdl2:+sdl-scancode-5+ :scancode-5)
    (#.sdl2:+sdl-scancode-6+ :scancode-6)
    (#.sdl2:+sdl-scancode-7+ :scancode-7)
    (#.sdl2:+sdl-scancode-8+ :scancode-8)
    (#.sdl2:+sdl-scancode-9+ :scancode-9)
    (#.sdl2:+sdl-scancode-0+ :scancode-0)

    (#.sdl2:+sdl-scancode-return+ :scancode-return)
    (#.sdl2:+sdl-scancode-escape+ :scancode-escape)
    (#.sdl2:+sdl-scancode-backspace+ :scancode-backspace)
    (#.sdl2:+sdl-scancode-tab+ :scancode-tab)
    (#.sdl2:+sdl-scancode-space+ :scancode-space)

    (#.sdl2:+sdl-scancode-minus+ :scancode-minus)
    (#.sdl2:+sdl-scancode-equals+ :scancode-equals)
    (#.sdl2:+sdl-scancode-leftbracket+ :scancode-leftbracket)
    (#.sdl2:+sdl-scancode-rightbracket+ :scancode-rightbracket)
    (#.sdl2:+sdl-scancode-backslash+ :scancode-backslash)

    (#.sdl2:+sdl-scancode-nonushash+ :scancode-nonushash)
    (#.sdl2:+sdl-scancode-semicolon+ :scancode-semicolon)
    (#.sdl2:+sdl-scancode-apostrophe+ :scancode-apostrophe)
    (#.sdl2:+sdl-scancode-grave+ :scancode-grave)
    (#.sdl2:+sdl-scancode-comma+ :scancode-comma)
    (#.sdl2:+sdl-scancode-period+ :scancode-period)
    (#.sdl2:+sdl-scancode-slash+ :scancode-slash)

    (#.sdl2:+sdl-scancode-capslock+ :scancode-capslock)

    (#.sdl2:+sdl-scancode-f1+ :scancode-f1)
    (#.sdl2:+sdl-scancode-f2+ :scancode-f2)
    (#.sdl2:+sdl-scancode-f3+ :scancode-f3)
    (#.sdl2:+sdl-scancode-f4+ :scancode-f4)
    (#.sdl2:+sdl-scancode-f5+ :scancode-f5)
    (#.sdl2:+sdl-scancode-f6+ :scancode-f6)
    (#.sdl2:+sdl-scancode-f7+ :scancode-f7)
    (#.sdl2:+sdl-scancode-f8+ :scancode-f8)
    (#.sdl2:+sdl-scancode-f9+ :scancode-f9)
    (#.sdl2:+sdl-scancode-f10+ :scancode-f10)
    (#.sdl2:+sdl-scancode-f11+ :scancode-f11)
    (#.sdl2:+sdl-scancode-f12+ :scancode-f12)

    (#.sdl2:+sdl-scancode-printscreen+ :scancode-printscreen)
    (#.sdl2:+sdl-scancode-scrolllock+ :scancode-scrolllock)
    (#.sdl2:+sdl-scancode-pause+ :scancode-pause)
    (#.sdl2:+sdl-scancode-insert+ :scancode-insert)
    (#.sdl2:+sdl-scancode-home+ :scancode-home)
    (#.sdl2:+sdl-scancode-pageup+ :scancode-pageup)
    (#.sdl2:+sdl-scancode-delete+ :scancode-delete)
    (#.sdl2:+sdl-scancode-end+ :scancode-end)
    (#.sdl2:+sdl-scancode-pagedown+ :scancode-pagedown)
    (#.sdl2:+sdl-scancode-right+ :scancode-right)
    (#.sdl2:+sdl-scancode-left+ :scancode-left)
    (#.sdl2:+sdl-scancode-down+ :scancode-down)
    (#.sdl2:+sdl-scancode-up+ :scancode-up)

    (#.sdl2:+sdl-scancode-numlockclear+ :scancode-numlockclear)
    (#.sdl2:+sdl-scancode-kp-divide+ :scancode-kp-divide)
    (#.sdl2:+sdl-scancode-kp-multiply+ :scancode-kp-multiply)
    (#.sdl2:+sdl-scancode-kp-minus+ :scancode-kp-minus)
    (#.sdl2:+sdl-scancode-kp-plus+ :scancode-kp-plus)
    (#.sdl2:+sdl-scancode-kp-enter+ :scancode-kp-enter)
    (#.sdl2:+sdl-scancode-kp-1+ :scancode-kp-1)
    (#.sdl2:+sdl-scancode-kp-2+ :scancode-kp-2)
    (#.sdl2:+sdl-scancode-kp-3+ :scancode-kp-3)
    (#.sdl2:+sdl-scancode-kp-4+ :scancode-kp-4)
    (#.sdl2:+sdl-scancode-kp-5+ :scancode-kp-5)
    (#.sdl2:+sdl-scancode-kp-6+ :scancode-kp-6)
    (#.sdl2:+sdl-scancode-kp-7+ :scancode-kp-7)
    (#.sdl2:+sdl-scancode-kp-8+ :scancode-kp-8)
    (#.sdl2:+sdl-scancode-kp-9+ :scancode-kp-9)
    (#.sdl2:+sdl-scancode-kp-0+ :scancode-kp-0)
    (#.sdl2:+sdl-scancode-kp-period+ :scancode-kp-period)

    (#.sdl2:+sdl-scancode-nonusbackslash+ :scancode-nonusbackslash)
    (#.sdl2:+sdl-scancode-application+ :scancode-application)
    (#.sdl2:+sdl-scancode-power+ :scancode-power)
    (#.sdl2:+sdl-scancode-kp-equals+ :scancode-kp-equals)
    (#.sdl2:+sdl-scancode-f13+ :scancode-f13)
    (#.sdl2:+sdl-scancode-f14+ :scancode-f14)
    (#.sdl2:+sdl-scancode-f15+ :scancode-f15)
    (#.sdl2:+sdl-scancode-f16+ :scancode-f16)
    (#.sdl2:+sdl-scancode-f17+ :scancode-f17)
    (#.sdl2:+sdl-scancode-f18+ :scancode-f18)
    (#.sdl2:+sdl-scancode-f19+ :scancode-f19)
    (#.sdl2:+sdl-scancode-f20+ :scancode-f20)
    (#.sdl2:+sdl-scancode-f21+ :scancode-f21)
    (#.sdl2:+sdl-scancode-f22+ :scancode-f22)
    (#.sdl2:+sdl-scancode-f23+ :scancode-f23)
    (#.sdl2:+sdl-scancode-f24+ :scancode-f24)
    (#.sdl2:+sdl-scancode-execute+ :scancode-execute)
    (#.sdl2:+sdl-scancode-help+ :scancode-help)
    (#.sdl2:+sdl-scancode-menu+ :scancode-menu)
    (#.sdl2:+sdl-scancode-select+ :scancode-select)
    (#.sdl2:+sdl-scancode-stop+ :scancode-stop)
    (#.sdl2:+sdl-scancode-again+ :scancode-again)
    (#.sdl2:+sdl-scancode-undo+ :scancode-undo)
    (#.sdl2:+sdl-scancode-cut+ :scancode-cut)
    (#.sdl2:+sdl-scancode-copy+ :scancode-copy)
    (#.sdl2:+sdl-scancode-paste+ :scancode-paste)
    (#.sdl2:+sdl-scancode-find+ :scancode-find)
    (#.sdl2:+sdl-scancode-mute+ :scancode-mute)
    (#.sdl2:+sdl-scancode-volumeup+ :scancode-volumeup)
    (#.sdl2:+sdl-scancode-volumedown+ :scancode-volumedown)

    (#.sdl2:+sdl-scancode-kp-comma+ :scancode-kp-comma)
    (#.sdl2:+sdl-scancode-kp-equalsas400+ :scancode-kp-equalsas400)

    (#.sdl2:+sdl-scancode-international1+ :scancode-international1)
    (#.sdl2:+sdl-scancode-international2+ :scancode-international2)
    (#.sdl2:+sdl-scancode-international3+ :scancode-international3)
    (#.sdl2:+sdl-scancode-international4+ :scancode-international4)
    (#.sdl2:+sdl-scancode-international5+ :scancode-international5)
    (#.sdl2:+sdl-scancode-international6+ :scancode-international6)
    (#.sdl2:+sdl-scancode-international7+ :scancode-international7)
    (#.sdl2:+sdl-scancode-international8+ :scancode-international8)
    (#.sdl2:+sdl-scancode-international9+ :scancode-international9)
    (#.sdl2:+sdl-scancode-lang1+ :scancode-lang1)
    (#.sdl2:+sdl-scancode-lang2+ :scancode-lang2)
    (#.sdl2:+sdl-scancode-lang3+ :scancode-lang3)
    (#.sdl2:+sdl-scancode-lang4+ :scancode-lang4)
    (#.sdl2:+sdl-scancode-lang5+ :scancode-lang5)
    (#.sdl2:+sdl-scancode-lang6+ :scancode-lang6)
    (#.sdl2:+sdl-scancode-lang7+ :scancode-lang7)
    (#.sdl2:+sdl-scancode-lang8+ :scancode-lang8)
    (#.sdl2:+sdl-scancode-lang9+ :scancode-lang9)

    (#.sdl2:+sdl-scancode-alterase+ :scancode-alterase)
    (#.sdl2:+sdl-scancode-sysreq+ :scancode-sysreq)
    (#.sdl2:+sdl-scancode-cancel+ :scancode-cancel)
    (#.sdl2:+sdl-scancode-clear+ :scancode-clear)
    (#.sdl2:+sdl-scancode-prior+ :scancode-prior)
    (#.sdl2:+sdl-scancode-return2+ :scancode-return2)
    (#.sdl2:+sdl-scancode-separator+ :scancode-separator)
    (#.sdl2:+sdl-scancode-out+ :scancode-out)
    (#.sdl2:+sdl-scancode-oper+ :scancode-oper)
    (#.sdl2:+sdl-scancode-clearagain+ :scancode-clearagain)
    (#.sdl2:+sdl-scancode-crsel+ :scancode-crsel)
    (#.sdl2:+sdl-scancode-exsel+ :scancode-exsel)

    (#.sdl2:+sdl-scancode-kp-00+ :scancode-kp-00)
    (#.sdl2:+sdl-scancode-kp-000+ :scancode-kp-000)
    (#.sdl2:+sdl-scancode-thousandsseparator+ :scancode-thousandsseparator)
    (#.sdl2:+sdl-scancode-decimalseparator+ :scancode-decimalseparator)
    (#.sdl2:+sdl-scancode-currencyunit+ :scancode-currencyunit)
    (#.sdl2:+sdl-scancode-currencysubunit+ :scancode-currencysubunit)
    (#.sdl2:+sdl-scancode-kp-leftparen+ :scancode-kp-leftparen)
    (#.sdl2:+sdl-scancode-kp-rightparen+ :scancode-kp-rightparen)
    (#.sdl2:+sdl-scancode-kp-leftbrace+ :scancode-kp-leftbrace)
    (#.sdl2:+sdl-scancode-kp-rightbrace+ :scancode-kp-rightbrace)
    (#.sdl2:+sdl-scancode-kp-tab+ :scancode-kp-tab)
    (#.sdl2:+sdl-scancode-kp-backspace+ :scancode-kp-backspace)
    (#.sdl2:+sdl-scancode-kp-a+ :scancode-kp-a)
    (#.sdl2:+sdl-scancode-kp-b+ :scancode-kp-b)
    (#.sdl2:+sdl-scancode-kp-c+ :scancode-kp-c)
    (#.sdl2:+sdl-scancode-kp-d+ :scancode-kp-d)
    (#.sdl2:+sdl-scancode-kp-e+ :scancode-kp-e)
    (#.sdl2:+sdl-scancode-kp-f+ :scancode-kp-f)
    (#.sdl2:+sdl-scancode-kp-xor+ :scancode-kp-xor)
    (#.sdl2:+sdl-scancode-kp-power+ :scancode-kp-power)
    (#.sdl2:+sdl-scancode-kp-percent+ :scancode-kp-percent)
    (#.sdl2:+sdl-scancode-kp-less+ :scancode-kp-less)
    (#.sdl2:+sdl-scancode-kp-greater+ :scancode-kp-greater)
    (#.sdl2:+sdl-scancode-kp-ampersand+ :scancode-kp-ampersand)
    (#.sdl2:+sdl-scancode-kp-dblampersand+ :scancode-kp-dblampersand)
    (#.sdl2:+sdl-scancode-kp-verticalbar+ :scancode-kp-verticalbar)
    (#.sdl2:+sdl-scancode-kp-dblverticalbar+ :scancode-kp-dblverticalbar)
    (#.sdl2:+sdl-scancode-kp-colon+ :scancode-kp-colon)
    (#.sdl2:+sdl-scancode-kp-hash+ :scancode-kp-hash)
    (#.sdl2:+sdl-scancode-kp-space+ :scancode-kp-space)
    (#.sdl2:+sdl-scancode-kp-at+ :scancode-kp-at)
    (#.sdl2:+sdl-scancode-kp-exclam+ :scancode-kp-exclam)
    (#.sdl2:+sdl-scancode-kp-memstore+ :scancode-kp-memstore)
    (#.sdl2:+sdl-scancode-kp-memrecall+ :scancode-kp-memrecall)
    (#.sdl2:+sdl-scancode-kp-memclear+ :scancode-kp-memclear)
    (#.sdl2:+sdl-scancode-kp-memadd+ :scancode-kp-memadd)
    (#.sdl2:+sdl-scancode-kp-memsubtract+ :scancode-kp-memsubtract)
    (#.sdl2:+sdl-scancode-kp-memmultiply+ :scancode-kp-memmultiply)
    (#.sdl2:+sdl-scancode-kp-memdivide+ :scancode-kp-memdivide)
    (#.sdl2:+sdl-scancode-kp-plusminus+ :scancode-kp-plusminus)
    (#.sdl2:+sdl-scancode-kp-clear+ :scancode-kp-clear)
    (#.sdl2:+sdl-scancode-kp-clearentry+ :scancode-kp-clearentry)
    (#.sdl2:+sdl-scancode-kp-binary+ :scancode-kp-binary)
    (#.sdl2:+sdl-scancode-kp-octal+ :scancode-kp-octal)
    (#.sdl2:+sdl-scancode-kp-decimal+ :scancode-kp-decimal)
    (#.sdl2:+sdl-scancode-kp-hexadecimal+ :scancode-kp-hexadecimal)

    (#.sdl2:+sdl-scancode-lctrl+ :scancode-lctrl)
    (#.sdl2:+sdl-scancode-lshift+ :scancode-lshift)
    (#.sdl2:+sdl-scancode-lalt+ :scancode-lalt)
    (#.sdl2:+sdl-scancode-lgui+ :scancode-lgui)
    (#.sdl2:+sdl-scancode-rctrl+ :scancode-rctrl)
    (#.sdl2:+sdl-scancode-rshift+ :scancode-rshift)
    (#.sdl2:+sdl-scancode-ralt+ :scancode-ralt)
    (#.sdl2:+sdl-scancode-rgui+ :scancode-rgui)

    (#.sdl2:+sdl-scancode-mode+ :scancode-mode)

    (#.sdl2:+sdl-scancode-audionext+ :scancode-audionext)
    (#.sdl2:+sdl-scancode-audioprev+ :scancode-audioprev)
    (#.sdl2:+sdl-scancode-audiostop+ :scancode-audiostop)
    (#.sdl2:+sdl-scancode-audioplay+ :scancode-audioplay)
    (#.sdl2:+sdl-scancode-audiomute+ :scancode-audiomute)
    (#.sdl2:+sdl-scancode-mediaselect+ :scancode-mediaselect)
    (#.sdl2:+sdl-scancode-www+ :scancode-www)
    (#.sdl2:+sdl-scancode-mail+ :scancode-mail)
    (#.sdl2:+sdl-scancode-calculator+ :scancode-calculator)
    (#.sdl2:+sdl-scancode-computer+ :scancode-computer)
    (#.sdl2:+sdl-scancode-ac-search+ :scancode-ac-search)
    (#.sdl2:+sdl-scancode-ac-home+ :scancode-ac-home)
    (#.sdl2:+sdl-scancode-ac-back+ :scancode-ac-back)
    (#.sdl2:+sdl-scancode-ac-forward+ :scancode-ac-forward)
    (#.sdl2:+sdl-scancode-ac-stop+ :scancode-ac-stop)
    (#.sdl2:+sdl-scancode-ac-refresh+ :scancode-ac-refresh)
    (#.sdl2:+sdl-scancode-ac-bookmarks+ :scancode-ac-bookmarks)

    (#.sdl2:+sdl-scancode-brightnessdown+ :scancode-brightnessdown)
    (#.sdl2:+sdl-scancode-brightnessup+ :scancode-brightnessup)
    (#.sdl2:+sdl-scancode-displayswitch+ :scancode-displayswitch)
    (#.sdl2:+sdl-scancode-kbdillumtoggle+ :scancode-kbdillumtoggle)
    (#.sdl2:+sdl-scancode-kbdillumdown+ :scancode-kbdillumdown)
    (#.sdl2:+sdl-scancode-kbdillumup+ :scancode-kbdillumup)
    (#.sdl2:+sdl-scancode-eject+ :scancode-eject)
    (#.sdl2:+sdl-scancode-sleep+ :scancode-sleep)

    (#.sdl2:+sdl-scancode-app1+ :scancode-app1)
    (#.sdl2:+sdl-scancode-app2+ :scancode-app2)

    (#.sdl2:+sdl-scancode-audiorewind+ :scancode-audiorewind)
    (#.sdl2:+sdl-scancode-audiofastforward+ :scancode-audiofastforward)))

(defun %mouse-val->sym (val)
  (ecase val
    (#.sdl2:+sdl-button-left+
     :mouse-button-left)
    (#.sdl2:+sdl-button-right+
     :mouse-button-right)
    (#.sdl2:+sdl-button-middle+
     :mouse-button-middle)
    (#.sdl2:+sdl-button-x1+
     :mouse-button-x1)
    (#.sdl2:+sdl-button-x2+
     :mouse-button-x2)))

(defun %mouse-state ()
  (cffi:with-foreign-objects ((x :int) (y :int))
    (let ((state (sdl2:sdl-get-mouse-state x y)))
      (values state (cffi:mem-ref x :int) (cffi:mem-ref y :int)))))

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

(defclass sdl2-input-manager (finalizable)
  ((game-controllers
    :type hash-table
    :initform (make-hash-table)
    :accessor game-controllers)))

(defmethod initialize-instance :after ((input sdl2-input-manager) &key &allow-other-keys)
  (event-subscribe
   (e_sdl-event (dispatcher.impl:impl (dispatcher:current-dispatcher)))
   input
   '%sdl2-input-manager.sdl-event))

(defmethod dispose ((input sdl2-input-manager))
  (event-unsubscribe
   (e_sdl-event (dispatcher.impl:impl (dispatcher:current-dispatcher)))
   input)
  (call-next-method))

(defun %sdl2-input-manager.sdl-event (input sdl-event
                                      &aux
                                        (sdl-event-type (sdl2:get-event-type sdl-event)))
  (case sdl-event-type
    ((:mousebuttondown :mousebuttonup)
     (let ((device
            :keyboard/mouse)
           (window-id
            (plus-c:c-ref
             sdl-event
             sdl2-ffi:sdl-event
             :button :window-id))
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
         (event-notify
          (input:e_mouse-button (input:current-input-manager))
          (make-instance
           'input:mouse-button-event-args
           :device device
           :window-id window-id
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
           (window-id
            (plus-c:c-ref
             sdl-event
             sdl2-ffi:sdl-event
             :motion :window-id))
           (mod (sdl2-ffi.functions:sdl-get-mod-state)))
       (multiple-value-bind (state x y)
           (%mouse-state)
         (event-notify
          (input:e_mouse-move (input:current-input-manager))
          (make-instance
           'input:mouse-event-args
           :device device
           :window-id window-id
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
           (window-id
            (plus-c:c-ref
             sdl-event
             sdl2-ffi:sdl-event
             :wheel :window-id))
           (mod (sdl2-ffi.functions:sdl-get-mod-state))
           (delta (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :wheel :y))
           (direction (plus-c:c-ref sdl-event sdl2-ffi:sdl-event :wheel :direction)))
       (when (= direction sdl2-ffi:+sdl-mousewheel-flipped+)
         (setf direction (- direction)))

       (multiple-value-bind (state x y)
           (%mouse-state)
         (event-notify
          (input:e_mouse-wheel (input:current-input-manager))
          (make-instance
           'input:mouse-wheel-event-args
           :device device
           :window-id window-id
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
     (let* ((window-id
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :window :window-id))
            (event-type
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
       (event-notify
        (input:e_window-event (input:current-input-manager))
        (make-instance
         'input:window-event-args
         :window-id window-id
         :event-type (%window-event->sym event-type)
         :data1 data1
         :data2 data2))))
    (:syswmevent)
    ((:keydown :keyup)
     (let* ((device
             :keyboard/mouse)
            (window-id
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :key :window-id))
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
       (event-notify
        (input:e_key (input:current-input-manager))
        (make-instance
         'input:key-event-args
         :device device
         :window-id window-id
         :alt (logtest mod sdl2-ffi:+kmod-alt+)
         :ctrl (logtest mod sdl2-ffi:+kmod-ctrl+)
         :shift (logtest mod sdl2-ffi:+kmod-shift+)
         :gui (logtest mod sdl2-ffi:+kmod-gui+)
         :key key
         :pressed key-pressed))))
    (:textinput
     (let* ((window-id
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :text :window-id))
            (text-ptr
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :text :text plus-c:&))
            (text
             (cffi:foreign-string-to-lisp
              text-ptr
              :max-chars 31
              :encoding :utf-8)))
       (event-notify
        (input:e_text-input (input:current-input-manager))
        (make-instance
         'input:text-input-event-args
         :window-id window-id
         :text text))))
    (:textediting)
    (:controllerdeviceadded
     (let* ((id
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :cdevice :which)))
       (when (sdl2:game-controller-p id)
         (let ((pad (sdl2:game-controller-open id)))
           (setf (gethash id (game-controllers input)) pad)))))
    (:controllerdeviceremoved
     (let* ((id
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :cdevice :which))
            (pad (gethash id (game-controllers input))))
       (sdl2:game-controller-close pad)
       (remhash id (game-controllers input))))
    ((:controllerbuttondown :controllerbuttonup)
     (let* ((id
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :cbutton :which))
            (button
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :cbutton :button))
            (button-sym
             (%controller-button->sym button)))
       (event-notify
        (input:e_controller-button (input:current-input-manager))
        (make-instance
         'input:controller-button-event-args
         :device id
         :button button-sym
         :button-pressed (eq sdl-event-type :controllerbuttondown)))))
    (:controlleraxismotion
     (let* ((id
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :caxis :which))
            (axis
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :caxis :axis))
            (axis-sym
             (%controller-axis->sym axis))
            (value
             (plus-c:c-ref
              sdl-event
              sdl2-ffi:sdl-event
              :caxis :value))
            (value-norm
             (float
              (if (minusp value)
                  (/ value 32768)
                  (/ value 32767)))))
       (event-notify
        (input:e_controller-axis (input:current-input-manager))
        (make-instance
         'input:controller-axis-event-args
         :device id
         :axis axis-sym
         :value value-norm))))))

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

(defun %controller-button->sym (val)
  (eswitch (val)
    (sdl2-ffi:+sdl-controller-button-a+
     :a)
    (sdl2-ffi:+sdl-controller-button-b+
     :b)
    (sdl2-ffi:+sdl-controller-button-x+
     :x)
    (sdl2-ffi:+sdl-controller-button-y+
     :y)
    (sdl2-ffi:+sdl-controller-button-back+
     :back)
    (sdl2-ffi:+sdl-controller-button-guide+
     :guide)
    (sdl2-ffi:+sdl-controller-button-start+
     :start)
    (sdl2-ffi:+sdl-controller-button-leftstick+
     :left-stick)
    (sdl2-ffi:+sdl-controller-button-rightstick+
     :right-stick)
    (sdl2-ffi:+sdl-controller-button-leftshoulder+
     :left-shoulder)
    (sdl2-ffi:+sdl-controller-button-rightshoulder+
     :right-shoulder)
    (sdl2-ffi:+sdl-controller-button-dpad-up+
     :dpad-up)
    (sdl2-ffi:+sdl-controller-button-dpad-down+
     :dpad-down)
    (sdl2-ffi:+sdl-controller-button-dpad-left+
     :dpad-left)
    (sdl2-ffi:+sdl-controller-button-dpad-right+
     :dpad-right)))

(defun %controller-axis->sym (val)
  (eswitch (val)
    (sdl2-ffi:+sdl-controller-axis-leftx+
     :left-x)
    (sdl2-ffi:+sdl-controller-axis-lefty+
     :left-y)
    (sdl2-ffi:+sdl-controller-axis-rightx+
     :right-x)
    (sdl2-ffi:+sdl-controller-axis-righty+
     :right-y)
    (sdl2-ffi:+sdl-controller-axis-triggerleft+
     :left-trigger)
    (sdl2-ffi:+sdl-controller-axis-triggerright+
     :right-trigger)))
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

(defclass sdl2-window-event-args ()
  ((window-id
    :type sdl2:window-id
    :initarg :window-id
    :reader window-id)
   (event-id
    :type sdl2:sdl-window-event-id
    :initarg :event-id
    :reader event-id)
   (data1
    :type integer
    :initarg :data1
    :reader data1)
   (data2
    :type integer
    :initarg :data2
    :reader data2)))

(defclass sdl2-input-manager (input:input-manager)
  ((e_sdl2-window-event
    :initform (make-instance 'event :name "sdl2-window-event")
    :reader e_sdl2-window-event)))

(defmethod initialize-instance :after ((input sdl2-input-manager) &key &allow-other-keys)
  (event-subscribe
   *e_sdl2-event*
   input
   '%sdl2-input-manager.sdl-event))

(defmethod dispose ((input sdl2-input-manager))
  (event-unsubscribe
   *e_sdl2-event*
   input))

(defun %mouse-val->sym (val)
  (switch (val)
    (sdl2:+sdl-button-left+
     :mouse-button-left)
    (sdl2:+sdl-button-right+
     :mouse-button-right)
    (sdl2:+sdl-button-middle+
     :mouse-button-middle)
    (sdl2:+sdl-button-x1+
     :mouse-button-x1)
    (sdl2:+sdl-button-x2+
     :mouse-button-x2)))

(defun %controller-button->sym (val)
  (switch (val)
    (sdl2:+sdl-controller-button-a+
     :a)
    (sdl2:+sdl-controller-button-b+
     :b)
    (sdl2:+sdl-controller-button-x+
     :x)
    (sdl2:+sdl-controller-button-y+
     :y)
    (sdl2:+sdl-controller-button-back+
     :back)
    (sdl2:+sdl-controller-button-guide+
     :guide)
    (sdl2:+sdl-controller-button-start+
     :start)
    (sdl2:+sdl-controller-button-leftstick+
     :left-stick)
    (sdl2:+sdl-controller-button-rightstick+
     :right-stick)
    (sdl2:+sdl-controller-button-leftshoulder+
     :left-shoulder)
    (sdl2:+sdl-controller-button-rightshoulder+
     :right-shoulder)
    (sdl2:+sdl-controller-button-dpad-up+
     :dpad-up)
    (sdl2:+sdl-controller-button-dpad-down+
     :dpad-down)
    (sdl2:+sdl-controller-button-dpad-left+
     :dpad-left)
    (sdl2:+sdl-controller-button-dpad-right+
     :dpad-right)))

(defun %controller-axis->sym (val)
  (switch (val)
    (sdl2:+sdl-controller-axis-leftx+
     :left-x)
    (sdl2:+sdl-controller-axis-lefty+
     :left-y)
    (sdl2:+sdl-controller-axis-rightx+
     :right-x)
    (sdl2:+sdl-controller-axis-righty+
     :right-y)
    (sdl2:+sdl-controller-axis-triggerleft+
     :left-trigger)
    (sdl2:+sdl-controller-axis-triggerright+
     :right-trigger)))

(defun %sdl2-input-manager.sdl-event (input sdl-event
                                      &aux
                                        (sdl-event-type (cffi:foreign-slot-value sdl-event 'sdl2:sdl-event 'sdl2:type)))
  (case sdl-event-type
    ((#.sdl2:+sdl-mousebuttondown+ #.sdl2:+sdl-mousebuttonup+)
     (let ((device :keyboard/mouse)
           (mod (sdl2:sdl-get-mod-state))
           (button
             (%mouse-val->sym
              (cffi:foreign-slot-value sdl-event 'sdl2:sdl-mouse-button-event 'sdl2:button)))
           (button-pressed
             (=
              (cffi:foreign-slot-value sdl-event 'sdl2:sdl-mouse-button-event 'sdl2:state)
              sdl2:+sdl-pressed+)))
       (multiple-value-bind (state x y)
           (%mouse-state)
         (event-notify
          (input:e_mouse-button input)
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
     (let ((device :keyboard/mouse)
           (mod (sdl2:sdl-get-mod-state)))
       (multiple-value-bind (state x y)
           (%mouse-state)
         (event-notify
          (input:e_mouse-move input)
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
           (delta (cffi:foreign-slot-value sdl-event 'sdl2:sdl-mouse-wheel-event 'sdl2:y))
           (direction (cffi:foreign-slot-value sdl-event 'sdl2:sdl-mouse-wheel-event 'sdl2:direction)))
       (when (= direction sdl2:+sdl-mousewheel-flipped+)
         (setf direction (- direction)))

       (multiple-value-bind (state x y)
           (%mouse-state)
         (event-notify
          (input:e_mouse-wheel input)
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
     (let* ((window-id
              (cffi:foreign-slot-value sdl-event 'sdl2:sdl-window-event 'sdl2:window-id))
            (event-id
              (cffi:foreign-slot-value sdl-event 'sdl2:sdl-window-event 'sdl2:event))
            (data1
              (cffi:foreign-slot-value sdl-event 'sdl2:sdl-window-event 'sdl2:data1))
            (data2
              (cffi:foreign-slot-value sdl-event 'sdl2:sdl-window-event 'sdl2:data2)))
       (event-notify
        (e_sdl2-window-event input)
        (make-instance 'sdl2-window-event-args :window-id window-id :event-id event-id :data1 data1 :data2 data2))))
    ((#.sdl2:+sdl-keydown+ #.sdl2:+sdl-keyup+)
     (let* ((device :keyboard/mouse)
            (keysym
              (cffi:foreign-slot-pointer sdl-event 'sdl2:sdl-keyboard-event 'sdl2:keysym))
            (mod (cffi:foreign-slot-value keysym 'sdl2:sdl-keysym 'sdl2:mod))
            (key (%scancode->sym
                  (cffi:foreign-slot-value keysym 'sdl2:sdl-keysym 'sdl2:scancode)))
            (key-pressed
              (= (cffi:foreign-slot-value sdl-event 'sdl2:sdl-keyboard-event 'sdl2:state)
                 sdl2:+sdl-pressed+))
            (repeat
              (/= 0 (cffi:foreign-slot-value sdl-event 'sdl2:sdl-keyboard-event 'sdl2:repeat))))
       (declare (ignore repeat))
       (event-notify
        (input:e_key input)
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
              (cffi:foreign-slot-pointer sdl-event 'sdl2:sdl-text-input-event 'sdl2:text))
            (text
              (cffi:foreign-string-to-lisp
               text-ptr
               :max-chars 31
               :encoding :utf-8)))
       (event-notify
        (input:e_text-input input)
        (make-instance
         'input:text-input-event-args
         :text text))))
    (#.sdl2:+sdl-textediting+)
    (#.sdl2:+sdl-controllerdeviceadded+
     (let* ((id (cffi:foreign-slot-value sdl-event 'sdl2:sdl-controller-device-event 'sdl2:which)))
       (when (sdl2:sdl-is-game-controller id)
         (let ((pad (sdl2:sdl-game-controller-open id)))
           (setf (gethash id (input:game-controllers input)) pad)))))
    (#.sdl2:+sdl-controllerdeviceremoved+
     (let* ((id (cffi:foreign-slot-value sdl-event 'sdl2:sdl-controller-device-event 'sdl2:which))
            (pad (gethash id (input:game-controllers input))))
       (sdl2:sdl-game-controller-close pad)
       (remhash id (input:game-controllers input))))
    ((#.sdl2:+sdl-controllerbuttondown+ #.sdl2:+sdl-controllerbuttonup+)
     (let* ((id (cffi:foreign-slot-value sdl-event 'sdl2:sdl-controller-button-event 'sdl2:which))
            (button (cffi:foreign-slot-value sdl-event 'sdl2:sdl-controller-button-event 'sdl2:button))
            (button-sym
              (%controller-button->sym button))
            (button-pressed
              (= sdl-event-type #.sdl2:+sdl-controllerbuttondown+)))
       (event-notify
        (input:e_controller-button input)
        (make-instance
         'input:controller-button-event-args
         :device id
         :button button-sym
         :button-pressed button-pressed))))
    (#.sdl2:+sdl-controlleraxismotion+
     (let* ((id (cffi:foreign-slot-value sdl-event 'sdl2:sdl-controller-axis-event 'sdl2:which))
            (axis (cffi:foreign-slot-value sdl-event 'sdl2:sdl-controller-axis-event 'sdl2:axis))
            (axis-sym (%controller-axis->sym axis))
            (value (cffi:foreign-slot-value sdl-event 'sdl2:sdl-controller-axis-event 'sdl2:value))
            (value-norm (float (/ value 32768))))
       (event-notify
        (input:e_controller-axis input)
        (make-instance
         'input:controller-axis-event-args
         :device id
         :axis axis-sym
         :value value-norm))))))

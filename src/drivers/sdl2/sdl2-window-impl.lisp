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

(in-package #:sol.drivers.sdl2)

(defclass sdl2-window-impl (finalizable)
  ((ui-window
    :type ui:window
    :initarg :ui-window
    :initform (error "sdl2-window-impl: must supply ui-window")
    :reader ui-window)
   (sdl-window
    :type sdl2-ffi:sdl-window
    :reader sdl-window)
   (renderer
    :type media:renderer
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
(defgeneric sdl-window-event (impl event))
(defgeneric sdl-draw (impl))

(defmethod sdl-create-renderer ((impl sdl2-window-impl) sdl-window)
  (let ((sdl-renderer
         (sdl2:create-renderer
          sdl-window -1 (list :accelerated))))
    (sdl2:set-render-draw-blend-mode sdl-renderer sdl2-ffi:+sdl-blendmode-blend+)
    (make-instance 'media:sdl-renderer :native sdl-renderer)))

(defvar *%in-resize* nil)
(defvar *%in-closing* nil)
(defmethod sdl-window-event ((impl sdl2-window-impl) event)
  (case (input:event-type event)
    (:shown
     (sdl-draw impl))
    (:hidden)
    (:exposed
     (sdl-draw impl))
    (:moved
     (setf (sdl-window-l impl) (input:data1 event)
           (sdl-window-t impl) (input:data2 event)))
    (:resized
     (setf (sdl-window-w impl) (input:data1 event)
           (sdl-window-h impl) (input:data2 event))
     (let ((*%in-resize* t))
       (setf (ui:width (ui-window impl)) (sdl-window-w impl)
             (ui:height (ui-window impl)) (sdl-window-h impl)))
     (sdl-draw impl))
    (:size-changed
     (setf (sdl-window-w impl) (input:data1 event)
           (sdl-window-h impl) (input:data2 event))
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
             (ui:get-component-at-* (ui-window impl) (- x (ui.impl:window-impl-left impl)) (- y (ui.impl:window-impl-top impl))))))
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

    (let ((input (input:current-input-manager)))
      (event-subscribe
       (input:e_window-event input)
       impl
       '%sdl2-window-impl.window-event)))

(define-finalizer sdl2-window-impl (sdl-window renderer)
  (media:render-destroy renderer)
  (sdl2-ffi.functions:sdl-destroy-window sdl-window))

(defmethod dispose ((impl sdl2-window-impl))
  (let ((input (input:current-input-manager)))
    (event-unsubscribe
     (input:e_window-event input)
     impl))
  (unless *%in-closing*
    (ui.impl:impl-closed (ui-window impl)))
  (slot-makunbound impl 'ui-window)
  (call-next-method)
  (slot-makunbound impl 'renderer)
  (slot-makunbound impl 'sdl-window))

(defmethod ui.impl:window-impl-id ((impl sdl2-window-impl))
  (sdl2-ffi.functions:sdl-get-window-id (sdl-window impl)))

(defmethod ui.impl:window-impl-left ((impl sdl2-window-impl))
  (sdl-window-l impl))

(defmethod (setf ui.impl:window-impl-left) (value (impl sdl2-window-impl))
  (setf value (round value))
  (sdl2-ffi.functions:sdl-set-window-position
   (sdl-window impl)
   value
   sdl2-ffi:+sdl-windowpos-undefined+)
  (%sdl2-window-impl.refresh-pos impl)
  (setf value (sdl-window-l impl))
  value)

(defmethod ui.impl:window-impl-top ((impl sdl2-window-impl))
  (sdl-window-t impl))

(defmethod (setf ui.impl:window-impl-top) (value (impl sdl2-window-impl))
  (setf value (round value))
  (sdl2-ffi.functions:sdl-set-window-position
   (sdl-window impl)
   sdl2-ffi:+sdl-windowpos-undefined+
   value)
  (%sdl2-window-impl.refresh-pos impl)
  (setf value (sdl-window-t impl))
  value)

(defmethod ui.impl:window-impl-width ((impl sdl2-window-impl))
  (sdl-window-w impl))

(defmethod (setf ui.impl:window-impl-width) (value (impl sdl2-window-impl))
  (unless *%in-resize*
    (setf value (round value))
    (sdl2-ffi.functions:sdl-set-window-size
     (sdl-window impl)
     value
     (sdl-window-h impl))
    (%sdl2-window-impl.refresh-size impl))
  (sdl-window-h impl))

(defmethod ui.impl:window-impl-height ((impl sdl2-window-impl))
  (sdl-window-h impl))

(defmethod (setf ui.impl:window-impl-height) (value (impl sdl2-window-impl))
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

(defun %sdl2-window-impl.window-event (impl args)
  (when (= (ui.impl:window-impl-id impl) (input:window-id args))
    (sdl-window-event impl args)))
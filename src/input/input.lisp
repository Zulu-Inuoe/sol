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

(in-package #:sol.input)

(defclass window-event-args ()
  ((window-id
    :initarg :window-id
    :reader window-id)
   (event-type
    :type keyword
    :initarg :event-type
    :reader event-type)
   (data1
    :initarg :data1
    :reader data1)
   (data2
    :initarg :data2
    :reader data2)))

(defclass text-input-event-args ()
  ((window-id
    :initarg :window-id
    :reader window-id)
   (text
    :type string
    :initform ""
    :initarg :text
    :reader text)))

(defclass input-event-args ()
  ((device
    :initarg :device
    :reader device)))

(defclass key/mouse-event-args (input-event-args)
  ((window-id
    :initarg :window-id
    :reader window-id)
   (alt
    :type boolean
    :initform nil
    :initarg :alt
    :reader alt)
   (ctrl
    :type boolean
    :initform nil
    :initarg :ctrl
    :reader ctrl)
   (shift
    :type boolean
    :initform nil
    :initarg :shift
    :reader shift)
   (gui
    :type boolean
    :initform nil
    :initarg :gui
    :reader gui)))

(defclass key-event-args (key/mouse-event-args)
  ((key
    :type keyword
    :initarg :key
    :reader key)
   (key-pressed
    :type boolean
    :initarg :pressed
    :reader key-pressed)))

(defclass mouse-event-args (key/mouse-event-args)
  ((x
    :type integer
    :initarg :x
    :reader x)
   (y
    :type integer
    :initarg :y
    :reader y)
   (left-button
    :type boolean
    :initarg :left-button
    :reader left-button)
   (middle-button
    :type boolean
    :initarg :middle-button
    :reader middle-button)
   (right-button
    :type boolean
    :initarg :right-button
    :reader right-button)
   (x1-button
    :type boolean
    :initarg :x1-button
    :reader x1-button)
   (x2-button
    :type boolean
    :initarg :x2-button
    :reader x2-button)))

(defclass mouse-button-event-args (mouse-event-args)
  ((button
    :type keyword
    :initarg :button :reader button)
   (button-pressed
    :type boolean
    :initarg :button-pressed :reader button-pressed)
   (click-count
    :type integer
    :initarg :click-count :reader click-count)))

(defclass mouse-wheel-event-args (mouse-event-args)
  ((delta
    :type integer
    :initarg :delta :reader delta)))

(defclass controller-button-event-args (input-event-args)
  ((button
    :type keyword
    :initarg :button
    :reader button)
   (button-pressed
    :type boolean
    :initarg :button-pressed
    :reader button-pressed)))

(defclass controller-axis-event-args (input-event-args)
  ((axis
    :type keyword
    :initarg :axis
    :reader axis)
   (value
    :type number
    :initarg :value
    :reader value)))

(defclass input-gesture ()
  ((modifiers
    :type integer
    :initarg :modifiers
    :initform 0
    :reader modifiers)))

(defclass key-gesture ()
  ((key
    :type symbol
    :initarg :key
    :reader key)
   (modifiers
    :type integer
    :initarg :modifiers
    :initform 0
    :reader modifiers)))

(deftype mouse-action ()
  '(member
    :none
    :left-click
    :left-double-click
    :middle-click
    :middle-double-click
    :right-click
    :right-double-click
    :wheel-click))

(defgeneric matches (gesture input-event-args)
  (:method (gesture args)
    (declare (ignore gesture args))
    nil))

(defclass mouse-gesture (input-gesture)
  ((mouse-action
    :type mouse-action
    :initarg :mouse-action
    :reader mouse-action)))

(defmethod matches ((gesture key-gesture) (args key-event-args))
  (and (eq (key gesture) (key args))
       (= (modifiers gesture) (modifiers args))))

(defun %mouse-args->mouse-action (args)
  (case (button args)
    (:left
     (cond
       ((= (click-count args) 1) :left-click)
       ((= (click-count args) 2) :left-double-click)
       (t :none)))
    (:middle
     (cond
       ((= (click-count args) 1) :middle-click)
       ((= (click-count args) 2) :middle-double-click)
       (t :none)))
    (:right
     (cond
       ((= (click-count args) 1) :right-click)
       ((= (click-count args) 2) :right-double-click)
       (t :none)))
    (t
     :none)))

(defmethod matches ((gesture mouse-gesture) (args mouse-event-args))
  (and (not (eq (mouse-action gesture) :none))
       (eq (mouse-action gesture) (%mouse-args->mouse-action args))
       (= (modifiers gesture) (modifiers args))))
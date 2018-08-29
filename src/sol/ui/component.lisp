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

(defclass thickness ()
  ((left
    :type number :initform 0
    :initarg :left :accessor left)
   (top
    :type number :initform 0
    :initarg :top :accessor top)
   (right
    :type number :initform 0
    :initarg :right :accessor right)
   (bottom
    :type number :initform 0
    :initarg :bottom :accessor bottom)))

(defmethod print-object ((o thickness) stream)
  (print-unreadable-object (o stream :type t)
    (format stream
            "~A, ~A, ~A, ~A"
            (left o) (top o) (right o) (bottom o))))

;;;A component is any sort of UI object that can be drawn on screen,
;;;which may have user interactivity (buttons or labels, respectively)
(defclass component (dispatcher:dispatcher-object)
  ((name
    :type (or null string) :initform nil
    :initarg :name :accessor name)
   (data-context
    :type t :initform nil
    :initarg :data-context
    :accessor data-context)
   (parent
    :type (or null component) :initform nil
    :initarg :parent :accessor parent)
   (x
    :type number :initform 0
    :initarg :x :accessor x)
   (y
    :type number :initform 0
    :initarg :y :accessor y)
   (min-width
    :type (or null number) :initform nil
    :initarg :min-width
    :accessor min-width)
   (min-height
    :type (or null number) :initform nil
    :initarg :min-height
    :accessor min-height)
   (width
    :type (or null number) :initform nil
    :initarg :width)
   (height
    :type (or null number) :initform nil
    :initarg :height)
   (max-width
    :type (or null number) :initform nil
    :initarg :max-width
    :accessor max-width)
   (max-height
    :type (or null number) :initform nil
    :initarg :max-height
    :accessor max-height)
   (desired-width
    :type number :initform 0
    :reader desired-width)
   (desired-height
    :type number :initform 0
    :reader desired-height)
   (actual-width
    :type number :initform 0
    :reader actual-width)
   (actual-height
    :type number :initform 0
    :reader actual-height)
   (horizontal-alignment
    :type symbol :initform :stretch
    :initarg :horizontal-alignment :accessor horizontal-alignment)
   (vertical-alignment
    :type symbol :initform :stretch
    :initarg :vertical-alignment :accessor vertical-alignment)
   (margin
    :type thickness :reader margin)
   (background
    :type media:color :initform media.colors:*white*
    :initarg :background :accessor background)
   (border-brush
    :type media:color
    :initarg :border-brush :accessor border-brush)
   (border-thickness
    :type thickness
    :reader border-thickness)
   (foreground
    :type media:color :initform media.colors:*black*
    :initarg :foreground :accessor foreground)
   (font-family
    :type string :initform media:*default-font-family*
    :initarg :font-family :accessor font-family)
   (font-style
    :type string :initform media:*default-font-style*
    :initarg :font-style :accessor font-style)
   (font-size
    :type number :initform media:*default-font-size*
    :initarg :font-size :accessor font-size)
   (is-loaded
    :type boolean :initform nil
    :reader is-loaded)
   (visible
    :type boolean :initform t
    :initarg :visible :accessor visible)
   (is-mouse-captured
    :type boolean :initform nil
    :initarg :is-mouse-captured :reader is-mouse-captured)
   (dock-panel.dock
    :type keyword
    :initform :left
    :initarg :dock-panel.dock
    :accessor dock-panel.dock)
   (e_got-focus
    :type routed-event
    :initform (make-instance 'routed-event :name "got-focus"))
   (e_lost-focus
    :type routed-event
    :initform (make-instance 'routed-event :name "lost-focus"))
   (e_key-down
    :type routed-event
    :initform (make-instance 'routed-event :name "key-down"))
   (e_key-up
    :type routed-event
    :initform (make-instance 'routed-event :name "key-up"))
   (e_text-input
    :type routed-event
    :initform (make-instance 'routed-event :name "text-input"))
   (e_mouse-down
    :type routed-event
    :initform (make-instance 'routed-event :name "mouse-down"))
   (e_mouse-up
    :type routed-event
    :initform (make-instance 'routed-event :name "mouse-up"))
   (e_mouse-enter
    :type routed-event
    :initform (make-instance 'routed-event :name "mouse-enter" :routing-strategy :direct))
   (e_mouse-move
    :type routed-event
    :initform (make-instance 'routed-event :name "mouse-move"))
   (e_mouse-leave
    :type routed-event
    :initform (make-instance 'routed-event :name "mouse-leave" :routing-strategy :direct))
   (e_mouse-wheel
    :type routed-event
    :initform (make-instance 'routed-event :name "mouse-wheel")))
  (:default-initargs
   :margin (make-instance 'thickness)
    :border-brush media.colors:*black*
    :border-thickness (make-instance 'thickness)))

(defun invalidate-arrange (comp)
  ;;Just go up to the top
  (loop
    :for parent := comp :then (parent parent)
    :while (parent parent)
    :finally
       (when parent
         (drivers:ensure-active-driver)
         (drivers:request-arrange (drivers:driver-layout-manager (drivers:active-driver)) parent))))

(defun add-handler (comp event-desc obj handler)
  (assert (%has-event comp event-desc))
  (event-subscribe
   (%desc->event comp event-desc)
   obj
   handler))

(defun remove-handler (comp event-desc obj-or-handler)
  (assert (%has-event comp event-desc))
  (event-unsubscribe
   (%desc->event comp event-desc) obj-or-handler))

(defun raise-event (comp event-desc args)
  (assert (%has-event comp event-desc))

  (let ((event (%desc->event comp event-desc)))
    ;;Notify the current object, no matter what
    (event-notify event args)

    (case (routing-strategy event)
      (:bubble
       ;;Go upwards to each parent that can handle it and notify them too
       (loop :for parent := (parent comp) :then (parent parent)
             :until (null parent)
             :if (%has-event parent event-desc)
               :do (event-notify (%desc->event parent event-desc) args)))
      (:direct)  ;Nothing else to be done
      (:tunnel
       ;;Build up a route to use
       (let ((route nil))
         (loop
           ;;Start at the bottom (source) and climb up
           :for curr-obj := (source args) :then (parent curr-obj)
           ;;Until we reach the start component
           :until (eq curr-obj comp)
           :if (null curr-obj)
             :do (error "This really should not happen")
           :else
             :if (%has-event curr-obj event-desc)
               :do (push (%desc->event curr-obj event-desc) route))

         ;;Started from the bottom now my whole team fucking here
         (dolist (ev route)
           (event-notify ev args)))))))

(defun capture-mouse (comp)
  (setf (capturing-component) comp))

(defun is-mouse-over (comp)
  (and
   (mouse-over-component)
   (or (eq comp (mouse-over-component))
       (has-parent-p (mouse-over-component) comp))))

(defun is-mouse-directly-over (comp)
  (eq comp (mouse-over-component)))

(defgeneric width (comp))
(defgeneric (setf width) (comp val))

(defgeneric height (comp))
(defgeneric (setf height) (comp val))

(defgeneric load-component (comp))
(defgeneric unload-component (comp))

(defun measure (comp available-width available-height)
  (let ((modded-w available-width)
        (modded-h available-height)
        (left+right (+ (left (margin comp))
                       (right (margin comp))))
        (top+bottom (+ (top (margin comp))
                       (bottom (margin comp)))))
    (multiple-value-bind (min-w max-w min-h max-h)
        (%min-max comp)
      ;;Subtract out space for margins
      (when modded-w
        (decf modded-w left+right))
      (when modded-h
        (decf modded-h top+bottom))
      ;;Clamp to min/max
      (setf modded-w (%clamp modded-w min-w max-w))
      (setf modded-h (%clamp modded-h min-h max-h))

      ;;Pass on the measure call to subclass
      (multiple-value-bind (des-w des-h)
          (measure-override comp modded-w modded-h)
        ;;Make sure at least min
        (setf des-w (max des-w min-w))
        (setf des-h (max des-h min-h))

        ;;Add back in margins
        (incf des-w left+right)
        (incf des-h top+bottom)

        ;;Make sure doesn't surpass available
        (when available-width
          (setf des-w (min des-w available-width)))
        (when available-height
          (setf des-h (min des-h available-height)))

        ;;Make sure it's at least zero
        (setf des-w (max des-w 0))
        (setf des-h (max des-h 0))

        (setf (slot-value comp 'desired-width) des-w)
        (setf (slot-value comp 'desired-height) des-h)
        (values des-w des-h)))))

(defun arrange (comp x y width height)
  (let* ((left (left (margin comp)))
         (top (top (margin comp)))
         (left+right (+ left (right (margin comp))))
         (top+bottom (+ top (bottom (margin comp))))
         (final-w
          (if (eq (horizontal-alignment comp) :stretch)
              (- width left+right)
              (desired-width comp)))
         (final-h
          (if (eq (vertical-alignment comp) :stretch)
              (- height top+bottom)
              (desired-height comp))))

    (multiple-value-bind (min-w max-w min-h max-h)
        (%min-max comp)
      (setf final-w (max final-w min-w))
      (when max-w
        (setf final-w (min final-w max-w)))
      (setf final-h (max final-h min-h))
      (when max-h
        (setf final-h (min final-h max-h)))

      (multiple-value-bind (used-w used-h)
          (arrange-override comp final-w final-h)
        (setf (slot-value comp 'actual-width) used-w)
        (setf (slot-value comp 'actual-height) used-h)

        (let ((client-w (%clamp used-w min-w max-w))
              (client-h (%clamp used-h min-h max-h))
              (parent-w (max (- width left+right) 0))
              (parent-h (max (- height top+bottom) 0)))
          (multiple-value-bind (off-x off-y)
              (%alignment-offsets
               (horizontal-alignment comp)
               (vertical-alignment comp)
               client-w client-h
               parent-w parent-h)
            (incf off-x (+ x left))
            (incf off-y (+ y top))

            (setf (slot-value comp 'x) off-x)
            (setf (slot-value comp 'y) off-y))))))
  (values))

(defgeneric measure-override (comp available-width available-height))
(defgeneric arrange-override (comp final-width final-height))
(defgeneric draw (comp renderer))

(defun list-parents (comp &optional until)
  (if until
      (loop
         :for p := comp :then (parent p)
         :while (and p (not (funcall until p))) :collect p)
      (loop
         :for p := comp :then (parent p)
         :while p :collect p)))

(defun has-parent-p (comp parent)
  (if (eq (parent comp) parent)
      t
      (and (parent comp)
           (has-parent-p (parent comp) parent))))

(defgeneric contains-*-p (comp x y))

(defun contains-absolute-*-p (comp x y)
  (loop
     :for curr := comp :then (parent curr)
     :if (null (parent curr))
     :return (let ((containing-comp (get-component-at-* curr x y)))
               (and
                containing-comp
                (or (eq containing-comp comp)
                    (has-parent-p containing-comp comp))))))

(defgeneric get-component-at-* (comp x y))

(defmethod initialize-instance :after ((comp component)
                                       &key
                                         (border-thickness nil)
                                         (margin nil)
                                         &allow-other-keys)
  (setf (slot-value comp 'border-thickness) (%coerce-thickness border-thickness))
  (setf (slot-value comp 'margin) (%coerce-thickness margin)))

(defmethod width ((comp component))
  (slot-value comp 'width))

(defmethod (setf width) (val (comp component))
  (setf (slot-value comp 'width) val))

(defmethod height ((comp component))
  (slot-value comp 'height))

(defmethod (setf height) (val (comp component))
  (setf (slot-value comp 'height) val))

;;X and Y are relative to the component itself
(defmethod contains-*-p ((comp component) (x number) (y number))
  (and (get-component-at-* comp x y)
       t))

(defmethod get-component-at-* ((comp component) x y)
  (if (and (>= x 0)
           (>= y 0)
           (< x (actual-width comp))
           (< y (actual-height comp))
           (visible comp))
      (values comp x y)
      nil))

(defmethod load-component ((comp component))
  (declare (ignore comp)))

(defmethod load-component :around ((comp component))
  (unless (is-loaded comp)
    (call-next-method)
    (setf (slot-value comp 'is-loaded) t))
  (values))

(defmethod unload-component ((comp component))
  (declare (ignore comp)))

(defmethod unload-component :around ((comp component))
  (when (is-loaded comp)
    (call-next-method)
    (setf (slot-value comp 'is-loaded) nil))
  (values))

(defmethod draw :before ((comp component) renderer)
  (unless (or (not (visible comp))
              (zerop (actual-width comp))
              (zerop (actual-height comp)))
    (media:render-draw-rect
     renderer
     0 0
     (actual-width comp) (actual-height comp)
     :fill (background comp)
     :stroke (border-brush comp)
     ;;TODO Use thickness of each border separately
     :stroke-thickness (left (border-thickness comp)))))

(defmethod draw :around ((comp component) renderer)
  (unless (or (not (visible comp))
              (zerop (actual-width comp))
              (zerop (actual-height comp)))
    (media:render-push-translate renderer (truncate (x comp)) (truncate (y comp)))
    (unwind-protect
         (call-next-method)
      (media:render-pop renderer))))

(defmethod measure-override ((comp component) available-width available-height)
  (let ((des-w 0)
        (des-h 0))
    (values des-w des-h)))

(defmethod arrange-override ((comp component) final-width final-height)
  (values final-width final-height))

(defun %clamp (val min max)
  (when val
    (setf val (max val min)))

  (when max
    (setf val (if val (min val max) max)))
  val)

(defun %min-max (comp)
  (let ((min-width (min-width comp))
        (width (width comp))
        (max-width (max-width comp))
        (min-height (min-height comp))
        (height (height comp))
        (max-height (max-height comp)))
    (when (null min-width)
      (cond
        ((and width max-width)
         (setf min-width (min width max-width)))
        (width
         (setf min-width width))
        (max-width
         (setf min-width max-width))
        (t
         (setf min-width 0))))
    (when (and (null max-width)
               width)
      (setf max-width (max width min-width)))

    (when (null min-height)
      (cond
        ((and height max-height)
         (setf min-height (min height max-height)))
        (height
         (setf min-height height))
        (max-height
         (setf min-height max-height))
        (t
         (setf min-height 0))))

    (when (and (null max-height)
               height)
      (setf max-height (max height min-height)))

    (values min-width max-width min-height max-height)))

(defun %alignment-offsets (horz-align vert-align c-w c-h p-w p-h)
  (let (x y)
    (case horz-align
      (:left
       (setf x 0))
      (:right
       (setf x (- p-w c-w)))
      ((:center :stretch)
       (setf x (/ (- p-w c-w) 2))))
    (case vert-align
      (:top
       (setf y 0))
      (:bottom
       (setf y (- p-h c-h)))
      ((:center :stretch)
       (setf y (/ (- p-h c-h) 2))))

    (values x y)))

(defun %has-event (comp event-desc)
  (assert (symbolp event-desc))
  (slot-exists-p comp event-desc))

(defun %desc->event (comp event-desc)
  (assert (symbolp event-desc))
  (slot-value comp event-desc))

(defun %coerce-thickness (thickness)
  (etypecase thickness
    (thickness thickness)
    (real
     (make-instance 'thickness :left thickness :top thickness :right thickness :bottom thickness))
    (string
     (let ((l (split-sequence:split-sequence #\, thickness)))
       (mapcon
        (lambda (e)
          (when (car e)
            (setf (car e)
                  (if (zerop (length (car e)))
                      nil
                      (or (parse-float:parse-float (car e) :junk-allowed t)
                          0)))))
        l)
       (%coerce-thickness l)))
    (sequence
     (let ((length (length thickness))
           left top right bottom)
       (when (>= length 1)
         (setf left (elt thickness 0)))
       (when (>= length 2)
         (setf top (elt thickness 1)))
       (when (>= length 3)
         (setf right (elt thickness 2)))
       (when (>= length 4)
         (setf bottom (elt thickness 3)))

       (when (null left)
         (setf left 0))

       (when (null right)
         (setf right left))
       (when (null top)
         (setf top left))
       (when (null bottom)
         (setf bottom top))

       (make-instance 'thickness :left left :top top :right right :bottom bottom)))))

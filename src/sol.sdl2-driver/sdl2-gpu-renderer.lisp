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

#+nil
(defmacro %with-sdl-color ((color media-color) &body body)
  (with-gensyms (mcolor)
    `(cffi:with-foreign-object (,color '(:struct sdl2:sdl-color))
       (let ((,mcolor ,media-color))
         (setf (cffi:foreign-slot-value ,color '(:struct sdl2:sdl-color) 'sdl2:r) (media:r ,mcolor)
               (cffi:foreign-slot-value ,color '(:struct sdl2:sdl-color) 'sdl2:g) (media:g ,mcolor)
               (cffi:foreign-slot-value ,color '(:struct sdl2:sdl-color) 'sdl2:b) (media:b ,mcolor)
               (cffi:foreign-slot-value ,color '(:struct sdl2:sdl-color) 'sdl2:a) (media:a ,mcolor)))
       ,@body)))

(defmacro %with-sdl-color ((color media-color) &body body)
  (with-gensyms (mcolor)
    `(let* ((,mcolor ,media-color)
            (,color
              (list
                'sdl2:r (media:r ,mcolor)
                'sdl2:g (media:g ,mcolor)
                'sdl2:b (media:b ,mcolor)
                'sdl2:a (media:a ,mcolor))))
       (declare (dynamic-extent ,color))
       ,@body)))

(defmacro %with-gpu-rects ((&rest vars) &body body)
  `(cffi:with-foreign-objects (,@(mapcar (lambda (spec) `(,(car spec) '(:struct sdl2-gpu:gpu-rect))) vars))
     ,@ (mapcar
         (lambda (spec)
           (destructuring-bind (name &key (x 0) (y 0) (w 0) (h 0))
               spec
             `(setf
               (cffi:foreign-slot-value ,name '(:struct sdl2-gpu:gpu-rect) 'sdl2-gpu:x) ,x
               (cffi:foreign-slot-value ,name '(:struct sdl2-gpu:gpu-rect) 'sdl2-gpu:y) ,y
               (cffi:foreign-slot-value ,name '(:struct sdl2-gpu:gpu-rect) 'sdl2-gpu:w) ,w
               (cffi:foreign-slot-value ,name '(:struct sdl2-gpu:gpu-rect) 'sdl2-gpu:h) ,h)))
         vars)
     ,@body))

(defclass sdl2-gpu-renderer ()
  ((%gpu-target
    :type cffi:foreign-pointer
    :initarg :gpu-target
    :initform (error "renderer: must supply gpu-target")
    :reader %gpu-target)
   (%gpu-render-text-cache
    :documentation "text <=> surface. Used to index into surface cache."
    :type hash-table
    :initform (trivial-garbage:make-weak-hash-table :weakness :key)
    :reader %gpu-render-text-cache)
   (%gpu-render-surface-cache
    :documentation "surface <=> gpu-image cache. Used for rendered text"
    :type hash-table
    :initform (make-hash-table)
    :reader %gpu-render-surface-cache)
   (%gpu-render-anon-target-cache
    :documentation "gpu-target cache with no key. Used to clean up on destroy"
    :type list
    :initform ()
    :accessor %gpu-render-anon-target-cache)
   (%matrix-stack
    :documentation "Our matrix stack. sdl2-gpu only has a max of 5."
    :type list
    :initform ()
    :accessor %gpu-render-matrix-stack)))

(defmethod initialize-instance :after ((renderer sdl2-gpu-renderer)
                                       &key
                                         &allow-other-keys))

(defmethod dispose ((renderer sdl2-gpu-renderer))
  (unless (slot-boundp renderer '%gpu-target)
    (error "gpu-renderer: renderer already destroyed ~A" renderer))
  (maphash
   (lambda (k v)
     (declare (ignore k))
     (format t "gpu-renderer: destroying gpu-image: ~A~%" v)
     (sdl2-gpu:gpu-free-image v))
   (%gpu-render-surface-cache renderer))
  (clrhash (%gpu-render-surface-cache renderer))

  (mapc
   (lambda (v)
     (format t "gpu-renderer: destroying anon gpu-target: ~A~%" v)
     (let ((gpu-image (cffi:foreign-slot-value v '(:struct sdl2-gpu:gpu-target) 'sdl2-gpu:image)))
       (sdl2-gpu:gpu-free-target v)
       (sdl2-gpu:gpu-free-image gpu-image)))
   (%gpu-render-anon-target-cache renderer))
  (setf (%gpu-render-anon-target-cache renderer) ())

  (sdl2-gpu:gpu-free-target (%gpu-target renderer))
  (slot-makunbound renderer '%gpu-target)
  (clrhash (%gpu-render-text-cache renderer)))

(defmethod media:renderer-size ((renderer sdl2-gpu-renderer)
                                &aux
                                  (gpu-target (%gpu-target renderer)))
  (cffi:with-foreign-objects ((w :uint16) (h :uint16))
    (sdl2-gpu:gpu-get-virtual-resolution gpu-target w h)
    (values (cffi:mem-ref w :uint16) (cffi:mem-ref h :uint16))))


(defmethod media:render-clear ((renderer sdl2-gpu-renderer)
                               &key
                                 (color media.colors:*transparent*)
                               &aux
                                 (gpu-target (%gpu-target renderer)))
  (sdl2-gpu:gpu-clear-rgba gpu-target (media:r color) (media:g color) (media:b color) (media:a color))
  (values))

(defmethod media:render-present ((renderer sdl2-gpu-renderer)
                                 &aux (gpu-target (%gpu-target renderer)))
  (sdl2-gpu:gpu-flip gpu-target)
  (values))

(defmethod media:render-pop ((renderer sdl2-gpu-renderer))
  (when (null (%gpu-render-matrix-stack renderer))
    (error "invalid pop"))

  (sdl2-gpu:gpu-matrix-mode sdl2-gpu:+gpu-modelview+)
  (let ((mat (pop (%gpu-render-matrix-stack renderer))))
    (sdl2-gpu:gpu-flush-blit-buffer)
    (sdl2-gpu:gpu-matrix-copy (sdl2-gpu:gpu-get-current-matrix) mat)
    (cffi:foreign-free mat))
  (values))

(defmethod media:render-push-translate ((renderer sdl2-gpu-renderer) x y)
  (%gpu-render-push-matrix renderer)
  (sdl2-gpu:gpu-translate (float x) (float y) 0.0)
  (values))

(defmethod media:render-push-rotate ((renderer sdl2-gpu-renderer) angle
                                     &key
                                       (x 0.0)
                                       (y 0.0))
  (%gpu-render-push-matrix renderer)
  (sdl2-gpu:gpu-rotate (coerce (%radians->degrees angle) 'single-float) (float x) (float y) 1.0)
  (values))

(defmethod media:render-push-scale ((renderer sdl2-gpu-renderer) scale-x scale-y)
  (%gpu-render-push-matrix renderer)
  (sdl2-gpu:gpu-scale (float scale-x) (float scale-y) 1.0)
  (values))

(defmethod media:render-draw-point ((renderer sdl2-gpu-renderer) x y
                                    &key
                                      (color media.colors:*black*)
                                    &aux
                                      (gpu-target (%gpu-target renderer)))
  (%with-sdl-color (sdl-color color)
    (sdl2-gpu:gpu-pixel gpu-target (float x) (1+ (float y)) sdl-color))
  (values))

(defmethod media:render-draw-line ((renderer sdl2-gpu-renderer) x1 y1 x2 y2
                                   &key
                                     (color media.colors:*black*)
                                     (thickness 1)
                                   &aux (gpu-target (%gpu-target renderer)))
  (let ((old-thickness (sdl2-gpu:gpu-set-line-thickness (float thickness))))
    (%with-sdl-color (sdl-color color)
      (sdl2-gpu:gpu-line gpu-target (float x1) (float y1) (float x2) (float y2) sdl-color))
    (sdl2-gpu:gpu-set-line-thickness old-thickness))
  (values))

(defmethod media:render-draw-rect ((renderer sdl2-gpu-renderer) x y width height
                                   &key
                                     (fill nil)
                                     (stroke nil)
                                     (stroke-thickness 1)
                                   &aux
                                     (gpu-target (%gpu-target renderer))
                                     (stroke-thickness (if stroke stroke-thickness 0))
                                     (stroke-x1 (float x))
                                     (stroke-y1 (float y))
                                     (stroke-x2 (float (+ stroke-x1 width)))
                                     (stroke-y2 (float (+ stroke-y1 height)))
                                     (fill-x1 (float (+ stroke-x1 stroke-thickness)))
                                     (fill-y1 (float (+ stroke-y1 stroke-thickness)))
                                     (fill-x2 (float (- stroke-x2 stroke-thickness)))
                                     (fill-y2 (float (- stroke-y2 stroke-thickness))))
  (when (and stroke (> stroke-thickness 0))
    (%with-sdl-color (sdl-color stroke)
      (sdl2-gpu:gpu-rectangle-filled
       gpu-target
       stroke-x1 stroke-y1 stroke-x2 stroke-y2 sdl-color)))
  (when fill
    (%with-sdl-color (sdl-color fill)
      (sdl2-gpu:gpu-rectangle-filled
       gpu-target
       fill-x1 fill-y1 fill-x2 fill-y2 sdl-color))))


(defmethod media:render-draw-ellipse ((renderer sdl2-gpu-renderer) x y rx ry
                                      &key
                                        (fill nil)
                                        (stroke nil)
                                        (stroke-thickness 1)
                                      &aux
                                        (gpu-target (%gpu-target renderer))
                                        (stroke-thickness (if stroke (float stroke-thickness) 0.0))
                                        (stroke-rx (float rx))
                                        (stroke-ry (float ry))
                                        (fill-rx (- rx stroke-thickness))
                                        (fill-ry (- ry stroke-thickness)))
  (when stroke
    (%with-sdl-color (sdl-color stroke)
      (sdl2-gpu:gpu-ellipse
       gpu-target
       (float x) (float y) stroke-rx stroke-ry 0.0 sdl-color)))
  (when fill
    (%with-sdl-color (sdl-color fill)
      (sdl2-gpu:gpu-ellipse-filled
       gpu-target
       (float x) (float y) fill-rx fill-ry 0.0 sdl-color))))

(defmethod media:render-draw-image ((renderer sdl2-gpu-renderer) image x y
                                    &key
                                      (src-x 0)
                                      (src-y 0)
                                      (src-width (- (media:image-width image) src-x))
                                      (src-height (- (media:image-height image) src-y))
                                      (dst-width src-width)
                                      (dst-height src-height)
                                      (flip nil))
  (let ((gpu-target (%gpu-target renderer))
        (gpu-image (%gpu-render-cache-surface-image renderer (sdl-surface (media:image-impl image))))
        (src-x (float src-x))
        (src-y (float src-y))
        (src-w (float src-width))
        (src-h (float src-height))
        (dst-x (float x))
        (dst-y (float y))
        (dst-w (float dst-width))
        (dst-h (float dst-height)))
    (%with-gpu-rects ((src :x src-x :y src-y :w src-w :h src-h)
                      (dst :x dst-x :y dst-y :w dst-w :h dst-h))
      (sdl2-gpu:gpu-blit-rect-x
       gpu-image src gpu-target dst
       0.0 0.0 0.0
       (case flip
         (:x sdl2-gpu:+gpu-flip-horizontal+)
         (:y sdl2-gpu:+gpu-flip-vertical+)
         (:both (logior sdl2-gpu:+gpu-flip-horizontal+ sdl2-gpu:+gpu-flip-vertical+))
         (t sdl2-gpu:+gpu-flip-none+))))))

(defmethod media:render-draw-text ((renderer sdl2-gpu-renderer) text x y)
  (let ((gpu-target (%gpu-target renderer))
        (gpu-image (%gpu-render-cache-text renderer text)))
    (sdl2-gpu:gpu-blit
     gpu-image (cffi:null-pointer) gpu-target
     (float x)
     (float y)))
  (values))

(defmethod media:render-get-target ((renderer sdl2-gpu-renderer))
  (cons renderer (%gpu-target renderer)))

(defmethod media:render-set-target ((renderer sdl2-gpu-renderer) target
                                    &aux
                                      (target-renderer (car target))
                                      (target-gpu-target (cdr target)))
  (unless (eq renderer target-renderer)
    (error "gpu-renderer: invalid render target for renderer '~A'" target))

  (setf (slot-value renderer '%gpu-target) target-gpu-target)
  (values))

(defmethod media:render-create-target ((renderer sdl2-gpu-renderer) width height &key (opacity 1.0))
  (let ((gpu-image (sdl2-gpu:gpu-create-image width height sdl2-gpu:+gpu-format-rgba+)))
    (sdl2-gpu:gpu-set-blending gpu-image sdl2-gpu:+gpu-true+)
    (sdl2-gpu:gpu-set-rgba gpu-image #xFF #xFF #xFF (truncate (* (clamp opacity 0 1) #xFF)))
    (sdl2-gpu:gpu-set-anchor gpu-image 0.0 0.0)

    (let ((target (cons renderer (sdl2-gpu:gpu-load-target gpu-image))))
      (push (cdr target) (%gpu-render-anon-target-cache renderer))
      target)))

(defmethod media:render-draw-target ((renderer sdl2-gpu-renderer) target x y
                                     &aux
                                       (target-renderer (car target))
                                       (target-gpu-target (cdr target)))
  (unless (eq renderer target-renderer)
    (error "gpu-renderer: invalid render target for renderer '~A'" target))

  (when target-gpu-target
    (let ((gpu-target (%gpu-target renderer))
          (target-gpu-image (cffi:foreign-slot-value target-gpu-target 'sdl2-gpu:gpu-target 'sdl2-gpu:image))
          (x (float x))
          (y (float y)))
      (sdl2-gpu:gpu-blit target-gpu-image (cffi:null-pointer) gpu-target x y)))
  (values))

(defmethod media:render-destroy-target ((renderer sdl2-gpu-renderer) target
                                        &aux
                                          (target-renderer (car target))
                                          (target-gpu-target (cdr target)))
  (unless (eq renderer target-renderer)
    (error "gpu-renderer: invalid render target for renderer '~A'" target))

  (when target-gpu-target
    (let ((target-gpu-image (cffi:foreign-slot-value target-gpu-target '(:struct sdl2-gpu:gpu-target) 'sdl2-gpu:image)))
      (sdl2-gpu:gpu-free-target target-gpu-target)
      (sdl2-gpu:gpu-free-image target-gpu-image))

    (setf (%gpu-render-anon-target-cache target-renderer)
          (delete target-gpu-target (%gpu-render-anon-target-cache target-renderer)))
    (setf (cdr target) nil))
  (values))

(defun %gpu-render-cache-surface-image (renderer surface
                                        &aux
                                          (map (%gpu-render-surface-cache renderer))
                                          (key surface))
  (let ((gpu-image (gethash key map)))
    (unless gpu-image
      (setf gpu-image (sdl2-gpu:gpu-copy-image-from-surface surface))
      (sdl2-gpu:gpu-set-blending gpu-image sdl2-gpu:+gpu-true+)
      (sdl2-gpu:gpu-set-anchor gpu-image 0.0 0.0)
      (sdl2-gpu:gpu-set-snap-mode gpu-image sdl2-gpu:+gpu-snap-none+)
      (setf (gethash key map) gpu-image))

    gpu-image))

(defun %gpu-render-uncache-surface-image (renderer surface
                                          &aux
                                            (map (%gpu-render-surface-cache renderer))
                                            (key surface))
  (let ((gpu-image (gethash key map)))
    (unless gpu-image
      (error "gpu-renderer: attempting to uncache unknown gpu-image '~A'" key))
    (sdl2-gpu:gpu-free-image gpu-image)
    (remhash key map))
  (values))

(defun %gpu-render-cache-text (renderer text
                               &aux
                                 (map (%gpu-render-text-cache renderer))
                                 (key text))
  (let ((surface (gethash key map)))
    (cond
      ((null surface)
       (setf (gethash key map) (sdl-surface (media:text-impl text))))
      ((not (eq surface (sdl-surface (media:text-impl text))))
       (%gpu-render-uncache-surface-image renderer surface)
       (setf (gethash key map) (sdl-surface (media:text-impl text)))))
    (setf surface (sdl-surface (media:text-impl text)))

    (%gpu-render-cache-surface-image renderer surface)))

(defun %gpu-render-push-matrix (renderer)
  (sdl2-gpu:gpu-matrix-mode sdl2-gpu:+gpu-modelview+)
  (let ((new-mat (cffi:foreign-alloc :float :count 16)))
    (sdl2-gpu:gpu-matrix-copy new-mat (sdl2-gpu:gpu-get-current-matrix))
    (push new-mat (%gpu-render-matrix-stack renderer)))
  (values))

(defun %radians->degrees (radians)
  (* radians (/ 180 pi)))

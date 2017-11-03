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

(defclass sdl2-gpu-renderer ()
  ((%gpu-target
    :type sdl2-ffi:gpu-target
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
     (sdl2-ffi.functions:gpu-free-image v))
   (%gpu-render-surface-cache renderer))
  (clrhash (%gpu-render-surface-cache renderer))

  (mapc
   (lambda (v)
     (format t "gpu-renderer: destroying anon gpu-target: ~A~%" v)
     (let ((gpu-image (plus-c:c-ref v sdl2-ffi:gpu-target :image)))
       (sdl2-ffi.functions:gpu-free-target v)
       (sdl2-ffi.functions:gpu-free-image gpu-image)))
   (%gpu-render-anon-target-cache renderer))
  (setf (%gpu-render-anon-target-cache renderer) ())

  (sdl2-ffi.functions:gpu-free-target (%gpu-target renderer))
  (slot-makunbound renderer '%gpu-target)
  (clrhash (%gpu-render-text-cache renderer)))

(defmethod media:renderer-size ((renderer sdl2-gpu-renderer)
                                &aux
                                  (gpu-target (%gpu-target renderer)))
  (cffi:with-foreign-objects ((w :uint16) (h :uint16))
    (sdl2-ffi.functions:gpu-get-virtual-resolution gpu-target w h)
    (values (cffi:mem-ref w :uint16) (cffi:mem-ref h :uint16))))


(defmethod media:render-clear ((renderer sdl2-gpu-renderer)
                               &key
                                 (color media.colors:*transparent*)
                               &aux
                                 (gpu-target (%gpu-target renderer)))
  (sdl2-ffi.functions:gpu-clear-rgba gpu-target (media:r color) (media:g color) (media:b color) (media:a color))
  (values))

(defmethod media:render-present ((renderer sdl2-gpu-renderer)
                                 &aux (gpu-target (%gpu-target renderer)))
  (sdl2-ffi.functions:gpu-flip gpu-target)
  (values))

(defmethod media:render-pop ((renderer sdl2-gpu-renderer))
  (when (null (%gpu-render-matrix-stack renderer))
    (error "invalid pop"))

  (sdl2-ffi.functions:gpu-matrix-mode sdl2-ffi:+gpu-modelview+)
  (let ((mat (pop (%gpu-render-matrix-stack renderer))))
    (sdl2-ffi.functions:gpu-flush-blit-buffer)
    (sdl2-ffi.functions:gpu-matrix-copy (sdl2-ffi.functions:gpu-get-current-matrix) mat)
    (cffi:foreign-free mat))
  (values))

(defmethod media:render-push-translate ((renderer sdl2-gpu-renderer) x y)
  (%gpu-render-push-matrix renderer)
  (sdl2-ffi.functions:gpu-translate (float x) (float y) 0.0)
  (values))

(defmethod media:render-push-rotate ((renderer sdl2-gpu-renderer) angle
                                     &key
                                       (x 0.0)
                                       (y 0.0))
  (%gpu-render-push-matrix renderer)
  (sdl2-ffi.functions:gpu-rotate (coerce (%radians->degrees angle) 'single-float) (float x) (float y) 1.0)
  (values))

(defmethod media:render-push-scale ((renderer sdl2-gpu-renderer) scale-x scale-y)
  (%gpu-render-push-matrix renderer)
  (sdl2-ffi.functions:gpu-scale (float scale-x) (float scale-y) 1.0)
  (values))

(defmethod media:render-draw-point ((renderer sdl2-gpu-renderer) x y
                                    &key
                                      (color media.colors:*black*)
                                    &aux
                                      (gpu-target (%gpu-target renderer)))
  (sdl2-ffi.functions:gpu-pixel gpu-target (float x) (1+ (float y)) (media:pack-color color))
  (values))

(defmethod media:render-draw-line ((renderer sdl2-gpu-renderer) x1 y1 x2 y2
                                   &key
                                     (color media.colors:*black*)
                                     (thickness 1)
                                   &aux (gpu-target (%gpu-target renderer)))
  (let ((old-thickness (sdl2-ffi.functions:gpu-set-line-thickness (float thickness))))
    (sdl2-ffi.functions:gpu-line gpu-target (float x1) (float y1) (float x2) (float y2) (media:pack-color color))
    (sdl2-ffi.functions:gpu-set-line-thickness old-thickness))
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
    (sdl2-ffi.functions:gpu-rectangle-filled
     gpu-target
     stroke-x1 stroke-y1 stroke-x2 stroke-y2 (media:pack-color stroke)))
  (when fill
    (sdl2-ffi.functions:gpu-rectangle-filled
     gpu-target
     fill-x1 fill-y1 fill-x2 fill-y2 (media:pack-color fill))))


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
    (sdl2-ffi.functions:gpu-ellipse-filled
     gpu-target
     (float x) (float y) stroke-rx stroke-ry 0.0 (media:pack-color stroke)))
  (when fill
    (sdl2-ffi.functions:gpu-ellipse-filled
     gpu-target
     (float x) (float y) fill-rx fill-ry 0.0 (media:pack-color fill))))

(defmethod media:render-draw-image ((renderer sdl2-gpu-renderer) image x y
                                    &key
                                      (width (image-width image))
                                      (height (image-height image))
                                      (flip nil))
  (let ((gpu-target (%gpu-target renderer))
        (gpu-image (%gpu-render-cache-surface-image renderer (sdl-surface (media:image-impl image))))
        (src-x (float 0))
        (src-y (float 0))
        (src-w (float width))
        (src-h (float height))
        (dst-x (float x))
        (dst-y (float y))
        (dst-w (float width))
        (dst-h (float height)))
    (plus-c:c-let ((src sdl2-ffi:gpu-rect)
                   (dst sdl2-ffi:gpu-rect))
      (setf (plus-c:c-ref src sdl2-ffi:gpu-rect :x) src-x
            (plus-c:c-ref src sdl2-ffi:gpu-rect :y) src-y
            (plus-c:c-ref src sdl2-ffi:gpu-rect :w) src-w
            (plus-c:c-ref src sdl2-ffi:gpu-rect :h) src-h)

      (setf (plus-c:c-ref dst sdl2-ffi:gpu-rect :x) dst-x
            (plus-c:c-ref dst sdl2-ffi:gpu-rect :y) dst-y
            (plus-c:c-ref dst sdl2-ffi:gpu-rect :w) dst-w
            (plus-c:c-ref dst sdl2-ffi:gpu-rect :h) dst-h)

      (sdl2-ffi.functions:gpu-blit-rect-x
       gpu-image src gpu-target dst
       0.0 0.0 0.0
       (case flip
         (:x sdl2-ffi:+gpu-flip-horizontal+)
         (:y sdl2-ffi:+gpu-flip-vertical+)
         (:both (logior sdl2-ffi:+gpu-flip-horizontal+ sdl2-ffi:+gpu-flip-vertical+))
         (t sdl2-ffi:+gpu-flip-none+))))))

(defmethod media:render-draw-text ((renderer sdl2-gpu-renderer) text x y)
  (let ((gpu-target (%gpu-target renderer))
        (gpu-image (%gpu-render-cache-text renderer text)))
    (sdl2-ffi.functions:gpu-blit
     gpu-image (cffi:null-pointer) gpu-target
     (float (+ x (/ (plus-c:c-ref gpu-image sdl2-ffi:gpu-image :w) 2)))
     (float (+ y (/ (plus-c:c-ref gpu-image sdl2-ffi:gpu-image :h) 2)))))
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
  (let ((gpu-image (sdl2-ffi.functions:gpu-create-image width height sdl2-ffi:+gpu-format-abgr+)))
    (sdl2-ffi.functions:gpu-set-blending gpu-image 1)
    (sdl2-ffi.functions:gpu-set-rgba gpu-image #xFF #xFF #xFF (truncate (* (clamp opacity 0 1) #xFF)))

    (let ((target (cons renderer (sdl2-ffi.functions:gpu-load-target gpu-image))))
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
          (target-gpu-image (plus-c:c-ref target-gpu-target sdl2-ffi:gpu-target :image)))
      (sdl2-ffi.functions:gpu-blit target-gpu-image (cffi:null-pointer) gpu-target x y)))
  (values))

(defmethod media:render-destroy-target ((renderer sdl2-gpu-renderer) target
                                        &aux
                                          (target-renderer (car target))
                                          (target-gpu-target (cdr target)))
  (unless (eq renderer target-renderer)
    (error "gpu-renderer: invalid render target for renderer '~A'" target))

  (when target-gpu-target
    (let ((target-gpu-image (plus-c:c-ref target-gpu-target sdl2-ffi:gpu-target :image)))
      (sdl2-ffi.functions:gpu-free-target target-gpu-target)
      (sdl2-ffi.functions:gpu-free-image target-gpu-image))

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
      (setf gpu-image (sdl2-ffi.functions:gpu-copy-image-from-surface surface))
      (sdl2-ffi.functions:gpu-set-blending gpu-image 1)
      (setf (gethash key map) gpu-image))

    gpu-image))

(defun %gpu-render-uncache-surface-image (renderer surface
                                          &aux
                                            (map (%gpu-render-surface-cache renderer))
                                            (key surface))
  (let ((gpu-image (gethash key map)))
    (unless gpu-image
      (error "gpu-renderer: attempting to uncache unknown gpu-image '~A'" key))
    (sdl2-ffi.functions:gpu-free-image gpu-image)
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
  (sdl2-ffi.functions:gpu-matrix-mode sdl2-ffi:+gpu-modelview+)
  (let ((new-mat (cffi:foreign-alloc :float :count 16)))
    (sdl2-ffi.functions:gpu-matrix-copy new-mat (sdl2-ffi.functions:gpu-get-current-matrix))
    (push new-mat (%gpu-render-matrix-stack renderer)))
  (values))

(defun %radians->degrees (radians)
  (* radians (/ 180 pi)))
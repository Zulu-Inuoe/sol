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

;;NOTE Not included at the moment. Requires bindings to the SDL_gfx library

(in-package #:sol.sdl2-driver)

(defmacro %with-sdl2-rects ((&rest vars) &body body)
  `(cffi:with-foreign-objects (,@(mapcar (lambda (spec) `(,(car spec) 'sdl2:sdl-rect)) vars))
     ,@ (mapcar
         (lambda (spec)
           (destructuring-bind (name &key (x 0) (y 0) (w 0) (h 0))
               spec
             `(setf
               (cffi:foreign-slot-value ,name 'sdl2:sdl-rect 'sdl2:x) ,x
               (cffi:foreign-slot-value ,name 'sdl2:sdl-rect 'sdl2:y) ,y
               (cffi:foreign-slot-value ,name 'sdl2:sdl-rect 'sdl2:w) ,w
               (cffi:foreign-slot-value ,name 'sdl2:sdl-rect 'sdl2:h) ,h)))
         vars)
     ,@body))

(defclass sdl2-sdl-renderer ()
  ((%renderer-native
    :type sdl2:sdl-renderer
    :initarg :native
    :initform (error "sdl-renderer: must supply renderer")
    :reader %renderer-native)
   (%transforms
    :type list
    :initform nil
    :accessor %transforms)
   (%sdl-render-text-cache
    :documentation "text <=> surface. Used to index into texture cache."
    :type hash-table
    :initform (trivial-garbage:make-weak-hash-table :weakness :key)
    :reader %sdl-render-text-cache)
   (%sdl-render-texture-cache
    :documentation "surface <=> texture cache. Used for images & rendered text"
    :type hash-table
    :initform (make-hash-table)
    :reader %sdl-render-texture-cache)
   (%sdl-render-anon-texture-cache
    :documentation "texture cache with no key. Used to clean up on destroy"
    :type list
    :initform ()
    :accessor %sdl-render-anon-texture-cache)))

(defmethod initialize-instance :after ((renderer sdl2-sdl-renderer) &key &allow-other-keys))

(defmethod dispose ((renderer sdl2-sdl-renderer))
  (maphash
   (lambda (k v)
     (format t "sdl-renderer: destroying texture ~a~%" k)
     (sdl2:sdl-destroy-texture v))
   (%sdl-render-texture-cache renderer))
  (clrhash (%sdl-render-texture-cache renderer))

  (mapc
   (lambda (v)
     (format t "sdl-renderer: destroying anon texture: ~a~%" v)
     (sdl2:sdl-destroy-texture (cdr v)))
   (%sdl-render-anon-texture-cache renderer))
  (setf (%sdl-render-anon-texture-cache renderer) ())

  (clrhash (%sdl-render-text-cache renderer))


  (sdl2:sdl-destroy-renderer (%renderer-native renderer))
  (slot-makunbound renderer '%renderer-native))

(defmethod media:renderer-size ((renderer sdl2-sdl-renderer))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2:sdl-get-renderer-output-size (%renderer-native renderer) w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defmacro with-transformed-points (renderer (&rest bind-pairs) &body body)
  (if bind-pairs
      (once-only (renderer)
        (loop
           :with res-body := (destructuring-bind (x y) (first bind-pairs)
                               `(multiple-value-bind (,x ,y)
                                    (%sdl-render-apply-transforms ,renderer ,x ,y)
                                  (setf ,x (round ,x)
                                        ,y (round ,y))
                                  ,@body))
           :for (x y) :in (cdr bind-pairs)
           :do
           (setf res-body
                 `(multiple-value-bind (,x ,y) (%sdl-render-apply-transforms ,renderer ,x ,y)
                    (setf ,x (round ,x)
                          ,y (round ,y))
                    ,res-body))
           :finally
           (return res-body)))
      `(progn
         ,renderer
         ,@body)))

(defmethod media:render-clear ((renderer sdl2-sdl-renderer)
                               &key
                                 (color media.colors:*transparent*)
                               &aux
                                 (sdl-renderer (%renderer-native renderer)))
  (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
    (sdl2:sdl-get-render-draw-color sdl-renderer r g b a)
    (sdl2:sdl-set-render-draw-color
     sdl-renderer
     (media:r color) (media:g color) (media:b color) (media:a color))
    (sdl2:sdl-render-clear sdl-renderer)
    (sdl2:sdl-set-render-draw-color
     sdl-renderer
     (cffi:mem-ref r :uint8)
     (cffi:mem-ref g :uint8)
     (cffi:mem-ref b :uint8)
     (cffi:mem-ref a :uint8))))

(defmethod media:render-present ((renderer sdl2-sdl-renderer))
  (sdl2:sdl-render-present (%renderer-native renderer)))

(defmethod media:render-pop ((renderer sdl2-sdl-renderer))
  (pop (%transforms renderer))
  (values))

(defmethod media:render-push-translate ((renderer sdl2-sdl-renderer) x y)
  (push
   (lambda (px py)
     (values (+ px x) (+ py y)))
   (%transforms renderer))
  (values))

(defmethod media:render-push-rotate ((renderer sdl2-sdl-renderer) angle &key (x 0) (y 0)
                                     &aux
                                       (cos (cos angle))
                                       (sin (sin angle)))
  (push
   (lambda (px py)
     (values (* (- px x) cos)
             (* (- py y) sin)))
   (%transforms renderer))
  (values))

(defmethod media:render-push-scale ((renderer sdl2-sdl-renderer) scale-x scale-y)
  (push
   (lambda (px py)
     (values (* px scale-x)
             (* py scale-y)))
   (%transforms renderer))
  (values))

(defmethod media:render-draw-point ((renderer sdl2-sdl-renderer) x y
                                    &key
                                      (color media.colors:*black*)
                                    &aux
                                      (sdl-renderer (%renderer-native renderer)))
  (with-transformed-points renderer
      ((x y))
    (sdl2-gfx:pixel-color sdl-renderer x y (media:pack-color color))))

(defmethod media:render-draw-line ((renderer sdl2-sdl-renderer) x1 y1 x2 y2
                                   &key
                                     (color media.colors:*black*)
                                     (thickness 1)
                                   &aux (sdl-renderer (%renderer-native renderer)))
  (setf thickness (round thickness))
  (with-transformed-points renderer
      ((x1 y1)
       (x2 y2))
    (if (= thickness 1)
        (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
          (sdl2:sdl-get-render-draw-color sdl-renderer r g b a)
          (sdl2:sdl-set-render-draw-color
           sdl-renderer
           (media:r color) (media:g color) (media:b color) (media:a color))
          (sdl2:sdl-render-draw-line sdl-renderer x1 y1 x2 y2)
          (sdl2:sdl-set-render-draw-color
           sdl-renderer
           (cffi:mem-ref r :uint8)
           (cffi:mem-ref g :uint8)
           (cffi:mem-ref b :uint8)
           (cffi:mem-ref a :uint8)))
        (sdl2-gfx:thick-line-color
         sdl-renderer
         x1 y1 x2 y2
         thickness (media:pack-color color)))))

(defmethod media:render-draw-rect ((renderer sdl2-sdl-renderer) x y width height
                                   &key
                                     (fill nil)
                                     (stroke nil)
                                     (stroke-thickness 1)
                                   &aux
                                     (sdl-renderer (%renderer-native renderer))
                                     (stroke-thickness (if stroke stroke-thickness 0))
                                     (stroke-x1 x)
                                     (stroke-y1 y)
                                     (stroke-x2 (+ stroke-x1 (1- width)))
                                     (stroke-y2 (+ stroke-y1 (1- height)))
                                     (fill-x1 (+ stroke-x1 stroke-thickness))
                                     (fill-y1 (+ stroke-y1 stroke-thickness))
                                     (fill-x2 (- stroke-x2 stroke-thickness))
                                     (fill-y2 (- stroke-y2 stroke-thickness)))
  (setf stroke-thickness (round stroke-thickness))
  (with-transformed-points renderer
      ((fill-x1 fill-y1)
       (fill-x2 fill-y2)
       (stroke-x1 stroke-y1)
       (stroke-x2 stroke-y2))
    (when (and stroke (> stroke-thickness 0))
      (sdl2-gfx:box-color
       sdl-renderer
       stroke-x1 stroke-y1 stroke-x2 stroke-y2 (media:pack-color stroke)))
    (when fill
      (sdl2-gfx:box-color
       sdl-renderer
       fill-x1 fill-y1 fill-x2 fill-y2 (media:pack-color fill)))))


(defmethod media:render-draw-ellipse ((renderer sdl2-sdl-renderer) x y rx ry
                                      &key
                                        (fill nil)
                                        (stroke nil)
                                        (stroke-thickness 1)
                                      &aux
                                        (sdl-renderer (%renderer-native renderer))
                                        (stroke-thickness (if stroke stroke-thickness 0))
                                        (stroke-rx (round rx))
                                        (stroke-ry (round ry))
                                        (fill-rx (round (- rx stroke-thickness)))
                                        (fill-ry (round (- ry stroke-thickness))))
  (setf stroke-thickness (round stroke-thickness))
  (with-transformed-points renderer
      ((x y))
    (when stroke
      (sdl2-gfx:filled-ellipse-color
       sdl-renderer
       x y stroke-rx stroke-ry (media:pack-color stroke)))
    (when fill
      (sdl2-gfx:filled-ellipse-color
       sdl-renderer
       x y fill-rx fill-ry (media:pack-color fill)))))

(defmethod media:render-draw-image ((renderer sdl2-sdl-renderer) image x y
                                    &key
                                      (src-x 0)
                                      (src-y 0)
                                      (src-width (- (media:image-width image) src-x))
                                      (src-height (- (media:image-height image) src-y))
                                      (dst-width src-width)
                                      (dst-height src-height)
                                      (flip nil))
  (when (null (sdl-surface (media:image-impl image)))
    (return-from media:render-draw-image))

  (let ((texture (%sdl-render-cache-surface-texture renderer (sdl-surface (media:image-impl image))))
        (src-x (round src-x))
        (src-y (round src-y))
        (src-w (round src-width))
        (src-h (round src-height))
        (dst-x x)
        (dst-y y)
        (dst-w (round dst-width))
        (dst-h (round dst-height)))
    (with-transformed-points renderer
        ((dst-x dst-y))
      (%with-sdl2-rects ((src :x src-x :y src-y :w src-w :h src-h)
                         (dst :x dst-x :y dst-y :w dst-w :h dst-h))
        (sdl2:sdl-render-copy-ex
         (%renderer-native renderer)
         texture
         src dst
         0.0d0 (cffi:null-pointer)
         (case flip
           (:x sdl2:+sdl-flip-horizontal+)
           (:y sdl2:+sdl-flip-vertical+)
           (:both (logior sdl2:+sdl-flip-horizontal+ sdl2:+sdl-flip-vertical+))
           (t sdl2:+sdl-flip-none+)))))))

(defmethod media:render-draw-text ((renderer sdl2-sdl-renderer) text x y)
  (when (null (sdl-surface (media:text-impl text)))
    (return-from media:render-draw-text))

  (let ((texture (%sdl-render-cache-text renderer text)))
    (with-transformed-points renderer
        ((x y))
      (%with-sdl2-rects ((dst :x x :y y
                              :w (media:text-width text)
                              :h (media:text-height text)))
        (sdl2:sdl-render-copy
         (%renderer-native renderer)
         texture
         (cffi:null-pointer)
         dst)))))

(defmethod media:render-get-target ((renderer sdl2-sdl-renderer))
  (cons renderer (sdl2:sdl-get-render-target (%renderer-native renderer))))

(defmethod media:render-set-target ((renderer sdl2-sdl-renderer) target
                                    &aux
                                      (target-renderer (car target))
                                      (target-texture (cdr target)))
  (unless (eq renderer target-renderer)
    (error "sdl-renderer: invalid render target for renderer '~A'" target))

  (sdl2:sdl-set-render-target (%renderer-native renderer) target-texture)
  (values))

(defmethod media:render-create-target ((renderer sdl2-sdl-renderer) width height &key (opacity 1))
  (let ((texture (sdl2:sdl-create-texture
                  (%renderer-native renderer)
                  sdl2:+sdl-pixelformat-argb8888+
                  sdl2:+sdl-textureaccess-target+
                  (ceiling width) (ceiling height))))
    (sdl2:sdl-set-texture-blend-mode texture sdl2:+sdl-blendmode-blend+)
    (sdl2:sdl-set-texture-alpha-mod texture (truncate (* (clamp opacity 0 1) #xFF)))

    (let ((target (cons renderer texture)))
      (push target (%sdl-render-anon-texture-cache renderer))
      target)))

(defmethod media:render-draw-target ((renderer sdl2-sdl-renderer) target x y
                                     &aux
                                       (target-renderer (car target))
                                       (target-texture (cdr target)))
  (unless (eq renderer target-renderer)
    (error "sdl-renderer: invalid render target for renderer '~A'" target))

  (with-transformed-points renderer
      ((x y))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (sdl2:sdl-query-texture target-texture (cffi:null-pointer) (cffi:null-pointer) w h)
      (%with-sdl2-rects ((dst :x x :y y :w (cffi:mem-ref w :int) :h (cffi:mem-ref h :int)))
        (sdl2:sdl-render-copy
         (%renderer-native renderer)
         target-texture
         (cffi:null-pointer) dst)))))

(defmethod media:render-destroy-target ((renderer sdl2-sdl-renderer) target
                                        &aux
                                          (target-renderer (car target))
                                          (target-texture (cdr target)))
  (when target-texture
    (setf (%sdl-render-anon-texture-cache target-renderer)
          (delete target-texture (%sdl-render-anon-texture-cache target-renderer)))
    (setf (cdr target) nil))
  (values))

(defun %sdl-render-apply-transforms (renderer x y)
  (loop
     :for tr :in (%transforms renderer)
     :do
     (setf (values x y) (funcall tr x y))
     :finally
     (return (values x y))))

(defun %sdl-render-cache-surface-texture (renderer surface
                                          &aux
                                            (map (%sdl-render-texture-cache renderer))
                                            (key surface))
  (let ((texture (gethash key map)))
    (unless texture
      (setf texture (sdl2:sdl-create-texture-from-surface (%renderer-native renderer) surface))
      (sdl2:sdl-set-texture-blend-mode texture sdl2:+sdl-blendmode-blend+)
      (setf (gethash key map) texture))

    texture))

(defun %sdl-render-uncache-surface-texture (renderer surface
                                            &aux
                                              (map (%sdl-render-texture-cache renderer))
                                              (key surface))
  (let ((texture (gethash key map)))
    (unless texture
      (error "sdl-renderer: attempting to uncache unknown texture '~A'" key))
    (sdl2:sdl-destroy-texture texture)
    (remhash key map))
  (values))

(defun %sdl-render-cache-text (renderer text
                               &aux
                                 (map (%sdl-render-text-cache renderer))
                                 (key text))
  (let ((surface (gethash key map)))
    (cond
      ((or (null surface))
       (setf (gethash key map) (sdl-surface (media:text-impl text))))
      ((not (eq surface (sdl-surface (media:text-impl text))))
       (%sdl-render-uncache-surface-texture renderer surface)
       (setf (gethash key map) (sdl-surface (media:text-impl text)))))
    (setf surface (sdl-surface (media:text-impl text)))

    (%sdl-render-cache-surface-texture renderer surface)))

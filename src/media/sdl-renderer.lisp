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

(in-package #:sol.media)

(defclass sdl-renderer (renderer)
  ((%renderer-native
    :type sdl2-ffi:sdl-renderer
    :initarg :native
    :initform (error "renderer: must supply renderer")
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

(defmethod initialize-instance :after ((renderer sdl-renderer)
                                       &key
                                         &allow-other-keys))

(defmethod renderer-size ((renderer sdl-renderer))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2-ffi.functions:sdl-get-renderer-output-size (%renderer-native renderer) w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defmethod renderer-width ((renderer sdl-renderer))
  (values (renderer-size renderer)))

(defmethod renderer-height ((renderer sdl-renderer))
  (nth-value 1 (renderer-size renderer)))

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

(defmethod render-destroy ((renderer sdl-renderer))
  (unless (slot-boundp renderer '%renderer-native)
    (error "renderer: renderer already destroyed ~A" renderer))
  (maphash
   (lambda (k v)
     (declare (ignore k))
     ;; (format t "renderer: destroying texture ~A~%" k)
     (sdl2-ffi.functions:sdl-destroy-texture v))
   (%sdl-render-texture-cache renderer))
  (mapc
   (lambda (v)
     (format t "renderer: destroying anon texture: ~A~%" v)
     (sdl2-ffi.functions:sdl-destroy-texture v))
   (%sdl-render-anon-texture-cache renderer))
  (sdl2:destroy-renderer (%renderer-native renderer))
  (slot-makunbound renderer '%renderer-native)
  (setf (%sdl-render-anon-texture-cache renderer) ())
  (clrhash (%sdl-render-texture-cache renderer))
  (clrhash (%sdl-render-text-cache renderer)))

(defmethod render-clear ((renderer sdl-renderer) &key (color *transparent*) &aux (sdl-renderer (%renderer-native renderer)))
  (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
    (sdl2-ffi.functions:sdl-get-render-draw-color sdl-renderer r g b a)
    (sdl2-ffi.functions:sdl-set-render-draw-color sdl-renderer (r color) (g color) (b color) (a color))
    (sdl2-ffi.functions:sdl-render-clear sdl-renderer)
    (sdl2-ffi.functions:sdl-set-render-draw-color
     sdl-renderer
     (cffi:mem-ref r :uint8)
     (cffi:mem-ref g :uint8)
     (cffi:mem-ref b :uint8)
     (cffi:mem-ref a :uint8))))

(defmethod render-present ((renderer sdl-renderer))
  (sdl2-ffi.functions:sdl-render-present (%renderer-native renderer)))

(defmethod render-pop ((renderer sdl-renderer))
  (pop (%transforms renderer))
  (values))

(defmethod render-push-translate ((renderer sdl-renderer) x y)
  (push
   (lambda (px py)
     (values (+ px x) (+ py y)))
   (%transforms renderer))
  (values))

(defmethod render-push-rotate ((renderer sdl-renderer) angle &key (x 0) (y 0)
                               &aux
                                 (cos (cos angle))
                                 (sin (sin angle)))
  (push
   (lambda (px py)
     (values (* (- px x) cos)
             (* (- py y) sin)))
   (%transforms renderer))
  (values))

(defmethod render-push-scale ((renderer sdl-renderer) scale-x scale-y)
  (push
   (lambda (px py)
     (values (* (- px origin-x) scale-x)
             (* (- py origin-y) scale-y)))
   (%transforms renderer))
  (values))

(defmethod render-draw-point ((renderer sdl-renderer) x y
                              &key
                                (color *black*)
                              &aux
                                (sdl-renderer (%renderer-native renderer)))
  (with-transformed-points renderer
      ((x y))
    (sdl2-ffi.functions:pixel-color sdl-renderer x y (pack-color color))))

(defmethod render-draw-line ((renderer sdl-renderer) x1 y1 x2 y2
                             &key
                               (color *black*)
                               (thickness 1)
                             &aux (sdl-renderer (%renderer-native renderer)))
  (setf thickness (round thickness))
  (with-transformed-points renderer
      ((x1 y1)
       (x2 y2))
    (if (= thickness 1)
        (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
          (sdl2-ffi.functions:sdl-get-render-draw-color sdl-renderer r g b a)
          (sdl2-ffi.functions:sdl-set-render-draw-color sdl-renderer (r color) (g color) (b color) (a color))
          (sdl2-ffi.functions:sdl-render-draw-line sdl-renderer x1 y1 x2 y2)
          (sdl2-ffi.functions:sdl-set-render-draw-color
           sdl-renderer
           (cffi:mem-ref r :uint8)
           (cffi:mem-ref g :uint8)
           (cffi:mem-ref b :uint8)
           (cffi:mem-ref a :uint8)))
        (sdl2-ffi.functions:thick-line-color
         sdl-renderer
         x1 y1 x2 y2 thickness (pack-color color)))))

(defmethod render-draw-rect ((renderer sdl-renderer) x y width height
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
      (sdl2-ffi.functions:box-color
       sdl-renderer
       stroke-x1 stroke-y1 stroke-x2 stroke-y2 (pack-color stroke)))
    (when fill
      (sdl2-ffi.functions:box-color
       sdl-renderer
       fill-x1 fill-y1 fill-x2 fill-y2 (pack-color fill)))))


(defmethod render-draw-ellipse ((renderer sdl-renderer) x y rx ry
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
      (sdl2-ffi.functions:filled-ellipse-color
       sdl-renderer
       x y stroke-rx stroke-ry (pack-color stroke)))
    (when fill
      (sdl2-ffi.functions:filled-ellipse-color
       sdl-renderer
       x y fill-rx fill-ry (pack-color fill)))))

(defmethod render-draw-image ((renderer sdl-renderer) image x y
                              &key
                                (width (image-width image))
                                (height (image-height image))
                                (flip nil))
  (when (null (%image-sdl-surface image))
    (return-from render-draw-image))

  (let ((texture (%sdl-render-cache-surface-texture renderer (%image-sdl-surface image)))
        (src-x (slot-value image '%src-x))
        (src-y (slot-value image '%src-y))
        (src-w (slot-value image '%src-w))
        (src-h (slot-value image '%src-h))
        (dst-x x)
        (dst-y y)
        (dst-w (round width))
        (dst-h (round height)))
    (with-transformed-points renderer
        ((dst-x dst-y))
      (sdl2:with-rects ((src src-x src-y src-w src-h)
                        (dst dst-x dst-y dst-w dst-h))
        (sdl2-ffi.functions:sdl-render-copy-ex
         (%renderer-native renderer)
         texture
         src dst
         0.0d0 (cffi:null-pointer)
         (case flip
           (:x sdl2-ffi:+sdl-flip-horizontal+)
           (:y sdl2-ffi:+sdl-flip-vertical+)
           (:both (logior sdl2-ffi:+sdl-flip-horizontal+ sdl2-ffi:+sdl-flip-vertical+))
           (t sdl2-ffi:+sdl-flip-none+)))))))

(defmethod render-draw-text ((renderer sdl-renderer) text x y)
  (when (null (%text-sdl-surface text))
    (return-from render-draw-text))

  (let ((texture (%sdl-render-cache-text renderer text)))
    (with-transformed-points renderer
        ((x y))
      (sdl2:with-rects ((dst x y
                             (text-width text)
                             (text-height text)))
        (sdl2-ffi.functions:sdl-render-copy
         (%renderer-native renderer)
         texture
         (cffi:null-pointer)
         dst)))))

(defmethod render-get-target ((renderer sdl-renderer))
  (cons renderer (sdl2-ffi.functions:sdl-get-render-target (%renderer-native renderer))))

(defmethod render-set-target ((renderer sdl-renderer) target
                              &aux
                                (target-renderer (car target))
                                (target-texture (cdr target)))
  (unless (eq renderer target-renderer)
    (error "renderer: invalid render target for renderer '~A'" target))

  (sdl2-ffi.functions:sdl-set-render-target (%renderer-native renderer) target-texture)
  (values))

(defmethod render-create-target ((renderer sdl-renderer) width height &key (opacity 1))
  (let ((texture (sdl2-ffi.functions:sdl-create-texture
                  (%renderer-native renderer)
                  sdl2-ffi:+sdl-pixelformat-argb8888+
                  sdl2-ffi:+sdl-textureaccess-target+
                  (ceiling width) (ceiling height))))
    (sdl2-ffi.functions:sdl-set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
    (sdl2-ffi.functions:sdl-set-texture-alpha-mod texture (truncate (* (clamp opacity 0 1) #xFF)))

    (let ((target (cons renderer texture)))
      (push target (%sdl-render-anon-texture-cache renderer))
      target)))

(defmethod render-draw-target ((renderer sdl-renderer) target x y
                               &aux
                                 (target-renderer (car target))
                                 (target-texture (cdr target)))
  (unless (eq renderer target-renderer)
    (error "renderer: invalid render target for renderer '~A'" target))

  (with-transformed-points renderer
      ((x y))
    (cffi:with-foreign-objects ((w :int) (h :int))
      (sdl2-ffi.functions:sdl-query-texture target-texture (cffi:null-pointer) (cffi:null-pointer) w h)
      (sdl2:with-rects ((dst x y (cffi:mem-ref w :int) (cffi:mem-ref h :int)))
        (sdl2-ffi.functions:sdl-render-copy
         (%renderer-native renderer)
         target-texture
         (cffi:null-pointer) dst)))))

(defmethod render-destroy-target ((renderer sdl-renderer) target
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
      (setf texture (sdl2-ffi.functions:sdl-create-texture-from-surface (%renderer-native renderer) surface))
      (sdl2-ffi.functions:sdl-set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
      (setf (gethash key map) texture))

    texture))

(defun %sdl-render-uncache-surface-texture (renderer surface
                                            &aux
                                              (map (%sdl-render-texture-cache renderer))
                                              (key surface))
  (let ((texture (gethash key map)))
    (unless texture
      (error "renderer: attempting to uncache unknown texture '~A'" key))
    (sdl2-ffi.functions:sdl-destroy-texture texture)
    (remhash key map))
  (values))

(defun %sdl-render-cache-text (renderer text
                               &aux
                                 (map (%sdl-render-text-cache renderer))
                                 (key text))
  (let ((surface (gethash key map)))
    (cond
      ((or (null surface))
       (setf (gethash key map) (%text-sdl-surface text)))
      ((not (eq surface (%text-sdl-surface text)))
       (%sdl-render-uncache-surface-texture renderer surface)
       (setf (gethash key map) (%text-sdl-surface text))))
    (setf surface (%text-sdl-surface text))

    (%sdl-render-cache-surface-texture renderer surface)))
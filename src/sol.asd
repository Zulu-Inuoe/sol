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

(defsystem #:sol
  :name "sol"
  :description "SOL Application framework"
  :version "0.0.0.1"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "zlib/libpng License <http://opensource.org/licenses/zlib-license.php>"
  :serial t
  :components
  ((:file "core/package")
   (:file "drivers/package")

   (:file "dispatcher/impl/package")
   (:file "dispatcher/package")

   (:file "input/package")

   (:file "media/package")

   (:file "ui/impl/package")
   (:file "ui/package")

   (:file "package")

   (:module "core"
    :serial t
    :components
    ((:file "event")
     (:file "dispose")
     (:file "finalizer")))

   (:module "drivers"
    :serial t
    :components
    ((:file "drivers")))

   (:module "dispatcher"
    :serial t
    :components
    ((:module "impl"
      :serial t
      :components
      ((:file "dispatcher-impl")))
     (:file "dispatcher")
     (:file "dispatcher-event")

     (:file "simple-dispatcher")
     (:file "current-dispatcher")))

   (:module "input"
    :serial t
    :components
    ((:file "input-map")
     (:file "input")
     (:file "mouse")
     (:file "input-manager")))

   (:module "media"
    :serial t
    :components
    ((:file "color")
     (:file "colors")

     (:file "font")
     (:file "text")

     (:file "image")

     (:file "renderer")

     (:file "animation")))

   (:module "ui"
    :serial t
    :components
    ((:module "impl"
      :serial t
      :components
      ((:file "window-impl")))
     (:file "command-base")
     (:file "delegate-command")

     (:file "observable-object")
     (:file "mouse")

     (:file "routed-event")
     (:file "focus-event-args")
     (:file "focus-manager")
     (:file "input-event-args")

     (:file "component")

     (:file "image")
     (:file "text-component")
     (:file "label")
     (:file "textbox")

     (:file "shapes")

     (:file "content-presenter")
     (:file "content-control")

     (:file "window")

     (:file "button")

     (:file "panel")
     (:file "stack-panel")
     (:file "dock-panel")
     (:file "grid")))

   ;;;app
   (:file "app")

   (:module "sdl2-driver"
    :serial t
    :components
    ((:file "package")
     (:file "sdl2-event")
     ;;dispatcher
     (:file "sdl2-dispatcher")

     ;;font
     (:file "sdl2-font-context")
     (:file "sdl2-font-impl")

     ;;text
     (:file "sdl2-text-impl")

     ;;image
     (:file "sdl2-image-context")
     (:file "sdl2-image-impl")

     ;;renderer
     (:file "sdl2-sdl-renderer")
     (:file "sdl2-gpu-renderer")

     (:file "sdl2-input-manager")

     ;;window impl
     (:file "sdl2-window-impl")
     (:file "sdl2-gpu-window-impl")

     ;;driver
     (:file "sdl2-gpu-driver")
     (:file "sdl2-driver"))))
  :depends-on
  (#:alexandria
   #:bordeaux-threads
   #:cffi
   #:cl-heap
   #:exit-hooks
   #:parse-float
   #:sdl2
   #:sdl2-gpu
   #:sdl2-gfx
   #:sdl2-image
   #:sdl2-ttf
   #:split-sequence
   #:trivial-features
   #:trivial-garbage

   #+win32
   #:win32))
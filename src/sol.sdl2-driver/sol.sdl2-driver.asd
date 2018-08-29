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

(defsystem #:sol.sdl2-driver
  :name "sol.sdl2-driver"
  :description "SDL2 driver for sol"
  :version "0.0.0.1"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "zlib/libpng License <http://opensource.org/licenses/zlib-license.php>"
  :serial t
  :components
  ((:file "package")
   (:file "sdl2")

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
   #+nil ;;not using atm
   (:file "sdl2-gpu-renderer")

   ;;window impl
   ;;(:file "sdl2-window-context")
   (:file "sdl2-window-impl")
   #+nil ;;not using atm
   (:file "sdl2-gpu-window-impl")

   (:file "sdl2-input-manager")

   ;;dispatcher
   #+os-windows
   (:file "message-only-hwnd")
   (:file "sdl2-dispatcher")

   ;;driver
   (:file "sdl2-driver")
   #+nil ;;not using atm
   (:file "sdl2-gpu-driver"))
  :depends-on
  (#:alexandria
   #:bordeaux-threads
   #:cffi
   #:defpackage-plus
   #:raw-bindings-sdl2
   #:raw-bindings-sdl2-image
   #:raw-bindings-sdl2-ttf
   #:raw-bindings-sdl2-gfx
   #+nil ;;not using atm
   #:raw-bindings-sdl2-gpu
   #:sol
   #:trivial-garbage
   #:xmls
   #+os-windows
   #:win32))

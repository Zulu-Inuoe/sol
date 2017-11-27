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
  :description "SDL2 Driver for SOL"
  :version "0.0.0.1"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "zlib/libpng License <http://opensource.org/licenses/zlib-license.php>"
  :serial t
  :components
  ((:file "package")
   (:file "sdl2-event")
   ;;dispatcher
   #-win32
   (:file "sdl2-dispatcher")
   #+win32
   (:file "sdl2-dispatcher-win32")

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
   (:file "sdl2-driver")
   (:file "sdl2-gpu-driver"))
  :depends-on
  (#:alexandria
   #:bordeaux-threads
   #:cffi
   #:sdl2
   #:sdl-gpu
   #:sdl2-gfx
   #:sdl2-image
   #:sdl2-ttf
   #:sol
   #:trivial-features
   #:trivial-garbage
   #:xmls

   #+win32
   #:win32))
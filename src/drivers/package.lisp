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

(defpackage #:sol.drivers
  (:use #:cl #:sol)
  (:export
   #:driver-class
   #:define-driver

   #:driver-dispatcher
   #:driver-window-impl
   #:driver-font-impl
   #:driver-text-impl
   #:driver-image-impl

   #:active-driver
   #:ensure-active-driver
   #:ensure-shutdown-driver

   ;;window-impl
   #:window-close
   #:window-left
   #:window-top
   #:window-width
   #:window-height
   #:window-draw-width
   #:window-draw-height

   ;;font-impl
   #:font-height
   #:font-size-text

   ;;text-impl
   #:text-width
   #:text-height
   #:text-set-dirty

   ;;image-impl
   #:image-width
   #:image-height))
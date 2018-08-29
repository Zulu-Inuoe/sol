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

(in-package #:defpackage+-user-1)

(defpackage+ #:sol
  (:use
   #:alexandria
   #:cl)
  (:export
   ;;;timespan
   #:timespan-from-millis
   #:timespan-from-seconds
   #:timespan-from-minutes
   #:timespan-from-hours
   #:timespan-from-time

   ;;;event
   #:event

   #:event-subscribe
   #:event-notify
   #:event-unsubscribe
   #:event-once

   ;;;dispose
   #:dispose
   #:with-disposeable
   #:dispose-on-error
   #:ensure-dispose

   ;;;disposable
   #:disposeable
   #:disposed

   ;;;finalizer
   #:finalizer
   #:define-finalizer

   ;;;finalizable
   #:finalizable

   ;;;app
   #:current-app
   #:app-start
   #:app-quit

   #:app
   #:windows
   #:main-window

   #:app-init
   #:app-add-window
   #:app-uninit))

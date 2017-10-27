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

(defpackage #:sol.dispatcher
  (:use #:alexandria #:cl #:sol.core)
  (:local-nicknames
   (#:drivers #:sol.drivers)
   (#:impl #:sol.dispatcher.impl))
  (:export
   ;;;dispatcher
   #:+priority.inactive+
   #:+priority.system-idle+
   #:+priority.application-idle+
   #:+priority.context-idle+
   #:+priority.background+
   #:+priority.input+
   #:+priority.loaded+
   #:+priority.render+
   #:+priority.databind+
   #:+priority.normal+
   #:+priority.send+

   #:check-access
   #:verify-access

   #:dispatcher-object
   #:dispatcher

   #:dispatcher-frame
   #:continue-frame

   #:dispatcher
   #:shutdown-started
   #:shutdown-finished
   #:e_unhandled-error

   #:current-dispatcher
   #:from-thread
   #:set-backend
   #:push-frame
   #:exit-all-frames
   #:run
   #:begin-invoke
   #:begin-invoke-shutdown
   #:invoke
   #:invoke-shutdown

   #:do-invoke

   #:dispatcher-unhandled-error-args
   #:args-error
   #:handled

   #:dispatcher-event))
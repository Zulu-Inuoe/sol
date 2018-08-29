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

(in-package #:sol.dispatcher)

(defclass dispatcher-event (event dispatcher-object)
  ()
  (:documentation
   "An event tied to a `dispatcher'.
  `event-subscribe' `event-notify', and `event-unsubscribed' may only be called by the thread on which the event was created."))

(defmethod event-subscribe :before ((event dispatcher-event) obj handler &key &allow-other-keys)
  "Verifies that the current thread has access to the specified event."
  (verify-access event))

(defmethod event-notify :before ((event dispatcher-event) arg)
  "Verifies that the current thread has access to the specified event."
  (verify-access event))

(defmethod event-unsubscribe :before ((event dispatcher-event) obj-or-handler)
  "Verifies that the current thread has access to the specified event."
  (verify-access event))

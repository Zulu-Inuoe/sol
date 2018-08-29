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

(in-package #:sol.dispatcher.impl)

(defgeneric wait-invoke-signal (dispatcher-impl)
  (:documentation "Requests the dispatcher-impl to wait until an invoke request is available."))

(defgeneric send-invoke-signal (dispatcher-impl)
  (:documentation "Requests that the dispatcher-impl be made aware of a new invoke request.
NOTE: This function may be called from any thread, including the dispatcher's own."))

(defgeneric send-shutdown-signal (dispatcher-impl)
  (:documentation "Requests that the dispatcher-impl be made aware that the shutdown process has begun."))

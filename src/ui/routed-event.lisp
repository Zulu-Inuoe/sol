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

(in-package #:sol.ui)

(defclass routed-event (event)
  ((routing-strategy
    :type keyword
    :initarg :routing-strategy
    :initform :bubble
    :reader routing-strategy)))

(defclass routed-event-args ()
  ((source
    :type component
    :initarg :source
    :initform (error "routed-event-args: must provide source")
    :reader source)
   (handled
    :type boolean
    :initform nil
    :accessor handled)))

(defmethod event-notify ((event routed-event) (args routed-event-args))
  (dolist (handler (sol.core::handlers event))
    (unless (handled args)
      (funcall (sol.core::handler handler) args)))

  (values))
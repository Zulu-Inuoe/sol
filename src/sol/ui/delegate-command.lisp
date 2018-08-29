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

(defclass delegate-command (command-base)
  ((obj
    :initform nil
    :initarg :obj
    :reader obj)
   (%can-execute
    :initform nil
    :initarg :can-execute
    :reader %can-execute)
   (%execute
    :initform nil
    :initarg :execute
    :reader %execute)))

(defmethod can-execute ((command delegate-command) arg)
  (if (%can-execute command)
      (if (obj command)
          (funcall (%can-execute command) (obj command) arg)
          (funcall (%can-execute command) arg))
      t))

(defmethod execute ((command delegate-command) arg)
  (when (%execute command)
    (if (obj command)
        (funcall (%execute command) (obj command) arg)
        (funcall (%execute command) arg))))

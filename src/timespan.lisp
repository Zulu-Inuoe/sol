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

(in-package #:sol)

(defun timespan-from-millis (ms)
  (timespan-from-time :millis ms))

(defun timespan-from-seconds (seconds)
  (timespan-from-time :seconds seconds))

(defun timespan-from-minutes (minutes)
  (timespan-from-time :minutes minutes))

(defun timespan-from-hours (hours)
  (timespan-from-time :hours hours))

(defun timespan-from-time (&key millis seconds minutes hours)
  (let ((timespan 0))
    (when millis
      (incf timespan (* (/ millis 1000) internal-time-units-per-second)))
    (when seconds
      (incf timespan (* seconds internal-time-units-per-second)))
    (when minutes
      (incf timespan (* (* minutes  60) internal-time-units-per-second)))
    (when hours
      (incf timespan (* (* hours 60 60) internal-time-units-per-second)))

    (values (round timespan))))
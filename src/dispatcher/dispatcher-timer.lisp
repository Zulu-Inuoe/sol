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

(defvar *%timer-thread* nil "Internal. Thread that schedules timers.")
(defvar *%timer-thread-running* nil "Internal. Whether or not the timer thread should be active. Effectively unused.")
(defvar *%timer-thread-lock* (bordeaux-threads:make-lock) "Internal. Guard timer thread state.")
(defvar *%timer-thread-cond* (bordeaux-threads:make-condition-variable) "Internal. Signal when new due time is updated.")
(defvar *%timer-thread-due-time* nil "Internal. Time at which the next timer is due in internal time units, or nil if no timer is scheduled.")
(defvar *%registered-timers* nil "Internal. All enabled timers.")

(defclass dispatcher-timer (dispatcher-object)
  ((interval
    :type integer
    :initarg :interval
    :initform 0
    :reader timer-interval
    :documentation "Interval between ticks, in timespan units")
   (e_tick
    :type dispatcher-event
    :initform (make-instance 'dispatcher-event :name "tick")
    :reader e_tick
    :documentation "While timer is active, event is invoked each time the interval elapses.")
   (%last-tick
    :type (or null integer)
    :initform nil
    :accessor %last-tick
    :documentation "Internal. Last time the timer ticked, or nil if the timer is disabled.")))

(defmethod (setf timer-interval) (value (timer dispatcher-timer))
  (verify-access timer)
  (bordeaux-threads:with-lock-held (*%timer-thread-lock*)
    (unless (= value (timer-interval timer))
      (setf (slot-value timer 'interval) value))
    (%timer-thread-update-due-time)
    (bordeaux-threads:condition-notify *%timer-thread-cond*)))

(defun timer-enabled-p (timer)
  (and (%last-tick timer) t))

(defun timer-start (timer)
  (verify-access timer)
  (%ensure-timer-thread)
  (bordeaux-threads:with-lock-held (*%timer-thread-lock*)
    (unless (%last-tick timer)
      (setf (%last-tick timer) (get-internal-real-time))
      (pushnew timer *%registered-timers*))
    (%timer-thread-update-due-time)
    (bordeaux-threads:condition-notify *%timer-thread-cond*))
  (values))

(defun timer-stop (timer)
  (verify-access timer)
  (bordeaux-threads:with-lock-held (*%timer-thread-lock*)
    (when (%last-tick timer)
      (setf (%last-tick timer) nil)
      (setf *%registered-timers* (delete timer *%registered-timers*)))
    (%timer-thread-update-due-time)
    (bordeaux-threads:condition-notify *%timer-thread-cond*))
  (values))

(defun %timer-due-p (timer
                      &aux (curr-time (get-internal-real-time)))
  (and (%last-tick timer)
       (<=  (- (+ (%last-tick timer) (timer-interval timer)) curr-time)
            0)))

(defun %ensure-lock-wait (timeout)
  (or
   (bordeaux-threads:condition-wait *%timer-thread-cond* *%timer-thread-lock* :timeout timeout)
   ;;If the wait wasn't successful we need to get the lock again
   (loop :until (bordeaux-threads:acquire-lock *%timer-thread-lock*))))

(defun %timer-thread-update-due-time ()
  ;;Update our next due time
  (loop
    :with due-time := nil
    :for timer :in *%registered-timers*
    :for timer-due-time := (and (%last-tick timer) (+ (%last-tick timer) (timer-interval timer)))
    :if (and timer-due-time
             (or (null due-time)
                 (< timer-due-time due-time)))
      :do (setf due-time timer-due-time)
    :finally
       (setf *%timer-thread-due-time* due-time)))

(defun %timer-thread-wait-due-time ()
  (loop
    :for time-left-seconds :=  (and *%timer-thread-due-time*
                                    (/ (- *%timer-thread-due-time* (get-internal-real-time))
                                       internal-time-units-per-second))
    :while (or (null time-left-seconds) (plusp time-left-seconds))
    :do (%ensure-lock-wait time-left-seconds)))

(defun %timer-thread-update (&aux due-timers)
  (bordeaux-threads:with-lock-held (*%timer-thread-lock*)
    (%timer-thread-update-due-time)
    (%timer-thread-wait-due-time)
    ;;Collect the due timers
    (setf due-timers
          (loop :for timer :in *%registered-timers*
                :when (%timer-due-p timer)
                  :collect timer)))

  ;;Trigger all due timers
  (let ((curr-time (get-internal-real-time)))
    (dolist (timer due-timers)
      (do-invoke ((dispatcher timer))
        (unwind-protect
             (event-notify (e_tick timer) nil)
          ;;If the timer is still active, schedule it
          (when (%last-tick timer)
            (setf (%last-tick timer) curr-time)))))))

(defun %timer-thread ()
  (loop
    :while *%timer-thread-running*
    :do (%timer-thread-update)))

(defun %ensure-timer-thread ()
  (bordeaux-threads:with-lock-held (*%timer-thread-lock*)
    (setf *%timer-thread-running* t)
    (unless (and *%timer-thread*
                 (bordeaux-threads:thread-alive-p *%timer-thread*))
      (setf *%timer-thread* (bordeaux-threads:make-thread #'%timer-thread)))))
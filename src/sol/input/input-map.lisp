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

(in-package #:sol.input)

;; Diagrams help
;; input-map:
;;   ((:keyboard/mouse
;;      (:space :jump
;;       :a :left
;;       :w :up
;;       :d :right
;;       :s :down)
;;    (:gamepad
;;      (:button :jump-a
;;       :dpad-left :left
;;       :dpad-up :up
;;       :dpat-right :right
;;       :dpad-down :down)))

(defclass control-scheme ()
  ((scheme-type
    :type symbol
    :initarg :scheme-type
    :reader scheme-type)
   (mappings
    :type hash-table
    :initform (make-hash-table :test 'eq)
    :reader mappings)))

(defmethod initialize-instance :after ((scheme control-scheme)
                                       &key (mappings ())
                                         &allow-other-keys)
  (dolist (mapping (plist-alist mappings))
    (push (cdr mapping) (gethash (car mapping) (mappings scheme))))

  (dolist (k (hash-table-keys (mappings scheme)))
    (setf (gethash k (mappings scheme))
          (nreverse (gethash k (mappings scheme))))))

(defun scheme-get-mapped-input-names (scheme input-name)
  (values (gethash input-name (mappings scheme))))

(defclass input-map ()
  ((active
    :type boolean
    :initarg :active
    :initform t
    :accessor active)
   (blocks
    :type boolean
    :initarg :blocks
    :initform nil
    :accessor blocks)
   (control-schemes
    :type hash-table
    :initform (make-hash-table)
    :reader control-schemes)))

(defparameter +known-scheme-types+ (list :keyboard/mouse :controller))

(defmethod initialize-instance :after ((input-map input-map) &key (mappings nil))
  (dolist (scheme-type +known-scheme-types+)
    (when-let ((scheme-data (getf mappings scheme-type)))
      (setf (gethash scheme-type (control-schemes input-map))
            (make-instance
             'control-scheme
             :scheme-type scheme-type
             :mappings scheme-data)))))

(defun map-get-mapped-input-names (input-map scheme-type input-name)
  (if-let ((scheme (gethash scheme-type (control-schemes input-map))))
    (scheme-get-mapped-input-names scheme input-name)
    ()))

(defclass input-map-chain ()
  ((input-maps
    :type list
    :initform nil
    :accessor input-maps))
  (:documentation
   "An `input-map-chain' holds a list of input-maps in a certain order.
Each `input-map' holds a set of control schemes, which map input names when active."))

(defmethod initialize-instance :after ((chain input-map-chain)
                                       &key
                                         (input-maps nil)
                                         &allow-other-keys)
  (setf (slot-value chain 'input-maps)
        (mapcar (lambda (m)
                  (make-instance 'input-map :mappings m))
                input-maps)))

(defun chain-get-mapped-input-names (input-map-chain scheme-type input-name)
  (loop
    :for map :in (input-maps input-map-chain)
    :if (active map)
      :appending (map-get-mapped-input-names map scheme-type input-name)
    :until (and (active map) (blocks map))))

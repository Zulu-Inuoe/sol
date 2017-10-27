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

(in-package #:sol.media)

(defun media-init ()
  (when *%media-initialized*
    (return-from media-init nil))

  (setf *%image-context* (make-instance '%image-context))
  (setf *%font-context* (make-instance '%font-context))
  (setf *%media-initialized* t))

(defun media-uninit ()
  (unless *%media-initialized*
    (return-from media-uninit t))

  (%font-context-destroy *%font-context*)
  (setf *%font-context* nil)

  (%image-context-destroy *%image-context*)
  (setf *%image-context* nil)
  (setf *%media-initialized* nil))

(defvar *%media-initialized* nil)
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

(in-package #:sol.sdl2-driver)

(defclass sdl2-input-manager (disposeable)
  ())

(defmethod initialize-instance :after ((input sdl2-input-manager) &key &allow-other-keys)
  (event-subscribe
   *e_sdl2-event*
   input
   '%sdl2-input-manager.sdl-event))

(defmethod dispose ((input sdl2-input-manager))
  (event-unsubscribe
   *e_sdl2-event*
   input))
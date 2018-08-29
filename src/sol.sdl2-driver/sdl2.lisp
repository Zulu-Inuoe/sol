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

(defvar *sdl2-invoke-event-type* nil
  "The event type for dispatcher invoke events on the SDL2 event system.")

(defvar *sdl2-shutdown-event-type* nil
  "The event type for dispatcher shutdown events on the SDL2 event system.")

(defvar *e_sdl2-event* (make-instance 'event :name "sdl2-event")
  "Raised when there's a new SDL2 event to process.")

(defvar %*sdl2-windows* (make-hash-table)
  "A mapping of window-id's to `tg:weak-pointer-p's pointing to `sdl2-window-impl's.")

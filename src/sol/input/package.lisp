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

(in-package #:defpackage+-user-1)

(defpackage+ #:sol.input
  (:use #:alexandria #:cl #:sol)
  (:local-nicknames
   (#:dispatcher #:sol.dispatcher)
   (#:drivers #:sol.drivers)
   (#:impl #:sol.ui.impl)
   (#:media #:sol.media)
   (#:media.colors #:sol.media.colors)
   (#:sdl2 #:raw-bindings-sdl2))
  (:export
   #:input-manager
   #:game-controllers
   #:e_mouse-move
   #:e_mouse-button
   #:e_mouse-wheel
   #:e_key
   #:e_text-input
   #:e_controller-button
   #:e_controller-axis
   #:current-input-manager

   #:get-mouse-cursor
   #:set-mouse-cursor
   #:capture-mouse
   #:release-mouse
   #:mouse-position

   #:text-input-event-args
   #:text

   #:input-event-args
   #:device

   #:key/mouse-event-args
   #:modifiers

   #:alt
   #:ctrl
   #:meta
   #:shift

   #:key-event-args
   #:key
   #:key-pressed

   #:mouse-event-args
   #:x
   #:y

   #:left-button-down
   #:middle-button-down
   #:right-button-down
   #:x-button-1-down
   #:x-button-2-down

   #:mouse-button-event-args
   #:button
   #:button-pressed
   #:click-count

   #:mouse-wheel-event-args
   #:delta

   #:controller-button-event-args
   #:button
   #:button-pressed

   #:controller-axis-event-args
   #:axis
   #:value

   #:input-gesture
   #:modifiers

   #:matches

   #:key-gesture
   #:key

   #:mouse-action

   #:mouse-gesture
   #:mouse-action

   #:player-handle
   #:global
   #:devices

   #:control-scheme

   #:scheme-get-mapped-input-names

   #:input-map
   #:active
   #:blocks
   #:control-schemes

   #:map-get-mapped-input-names

   #:input-map-chain
   #:input-maps

   #:chain-get-mapped-input-names))

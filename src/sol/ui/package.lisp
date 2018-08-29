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

(defpackage+ #:sol.ui
  (:use #:alexandria #:cl #:sol)
  (:local-nicknames
   (#:dispatcher #:sol.dispatcher)
   (#:drivers #:sol.drivers)
   (#:impl #:sol.ui.impl)
   (#:input #:sol.input)
   (#:media #:sol.media)
   (#:media.colors #:sol.media.colors)
   (#:sdl2 #:raw-bindings-sdl2))
  (:export
   ;;Global accessors
   #:active-focus-manager
   #:focused-component
   #:capturing-component
   #:mouse-over-component

   #:property-changed-event-args

   #:property-name
   #:observable-object
   #:e_property-changed

   #:set-property

   #:command-base
   #:e_can-execute-changed

   #:delegate-command

   #:can-execute
   #:execute

   #:thickness
   #:left
   #:top
   #:right
   #:bottom

   #:component
   #:name
   #:data-context
   #:x
   #:y
   #:width
   #:height
   #:max-width
   #:max-height
   #:min-width
   #:min-height
   #:desired-width
   #:desired-height
   #:actual-width
   #:actual-height
   #:horizontal-alignment
   #:vertical-alignment
   #:margin
   #:background
   #:foreground
   #:font-family
   #:font-style
   #:font-size
   #:is-loaded
   #:visible
   #:e_got-focus
   #:e_lost-focus
   #:e_key-down
   #:e_key-up
   #:e_text-input
   #:e_mouse-down
   #:e_mouse-up
   #:e_mouse-move
   #:e_mouse-wheel

   #:invalidate-arrange
   #:add-handler
   #:remove-handler
   #:raise-event
   #:contains-*-p
   #:contains-absolute-*-p
   #:get-component-at-*
   #:measure
   #:arrange
   #:draw

   #:shape
   #:shape-fill
   #:shape-stroke
   #:shape-stroke-thickness

   #:line
   #:x1
   #:y1
   #:x2
   #:y2

   #:rectangle

   #:ellipse

   #:image
   #:source
   #:stretch
   #:stretch-direction

   #:label
   #:text

   #:textbox
   #:is-enabled
   #:text
   #:e_text-changed

   #:content-control
   #:content
   #:horizontal-content-alignment
   #:vertical-content-alignment

   #:window
   #:window-left
   #:window-top
   #:window-size
   #:window-width
   #:window-height
   #:focus-manager
   #:e_window-closed

   #:window-close

   #:button
   #:command
   #:is-enabled
   #:clicked-color
   #:click-mode
   #:e_click

   #:panel
   #:children

   #:add-child
   #:remove-child

   #:stack-panel
   #:orientation
   #:spacing

   #:dock-panel

   #:grid

   #:routed-event
   #:routing-strategy

   #:routed-event-args
   #:source
   #:handled

   #:focus-event-args
   #:opposite-component))

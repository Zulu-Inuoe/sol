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

(defpackage #:sol.media
  (:use :alexandria #:cl #:sol)
  (:local-nicknames
   (#:drivers #:sol.drivers))
  (:export
   #:image
   #:image-path
   #:image-color-key
   #:image-width
   #:image-height
   #:image-impl

   #:frame
   #:frame-width
   #:frame-height

   #:animation
   #:name
   #:animation-width
   #:animation-height
   #:reset-animation
   #:update-animation
   #:load-animation
   #:load-animations

   #:font
   #:font-family
   #:font-style
   #:font-size
   #:font-bold
   #:font-italic
   #:font-underline
   #:font-strikethrough
   #:font-impl

   #:font-line-height
   #:font-size-text

   #:text
   #:text-string
   #:text-font
   #:text-color
   #:text-width
   #:text-height
   #:text-impl

   #:color
   #:r
   #:g
   #:b
   #:a

   #:color=
   #:color-*
   #:color-lerp
   #:pack-color
   #:pack-color-*
   #:unpack-color
   #:unpack-color-*

   ;;;renderer
   #:render-with-target
   #:render-with-new-target

   #:renderer-size
   #:renderer-width
   #:renderer-height

   #:render-clear
   #:render-present

   #:render-pop
   #:render-push-translate
   #:render-push-rotate
   #:render-push-scale

   #:render-draw-point
   #:render-draw-line
   #:render-draw-rect
   #:render-draw-ellipse
   #:render-draw-image
   #:render-draw-text

   #:render-create-target
   #:render-get-target
   #:render-set-target
   #:render-draw-target
   #:render-destroy-target))

(defpackage #:sol.media.colors
  (:use #:cl #:sol.media)
  (:export
   ;;Colors
   #:*alice-blue*
   #:*antique-white*
   #:*aqua*
   #:*aquamarine*
   #:*azure*
   #:*beige*
   #:*bisque*
   #:*black*
   #:*blanched-almond*
   #:*blue*
   #:*blue-violet*
   #:*brown*
   #:*burly-wood*
   #:*cadet-blue*
   #:*chartreuse*
   #:*chocolate*
   #:*coral*
   #:*cornflower-blue*
   #:*cornsilk*
   #:*crimson*
   #:*cyan*
   #:*dark-blue*
   #:*dark-cyan*
   #:*dark-goldenrod*
   #:*dark-gray*
   #:*dark-green*
   #:*dark-khaki*
   #:*dark-magenta*
   #:*dark-olivegreen*
   #:*dark-orange*
   #:*dark-orchid*
   #:*dark-red*
   #:*dark-salmon*
   #:*dark-seagreen*
   #:*dark-slateblue*
   #:*dark-slategray*
   #:*dark-turquoise*
   #:*dark-violet*
   #:*deep-pink*
   #:*deep-skyblue*
   #:*dim-gray*
   #:*dodger-blue*
   #:*firebrick*
   #:*floral-white*
   #:*forest-green*
   #:*fuchsia*
   #:*gainsboro*
   #:*ghost-white*
   #:*gold*
   #:*goldenrod*
   #:*gray*
   #:*green*
   #:*green-yellow*
   #:*honeydew*
   #:*hot-pink*
   #:*indian-red*
   #:*indigo*
   #:*ivory*
   #:*khaki*
   #:*lavender*
   #:*lavender-blush*
   #:*lawn-green*
   #:*lemon-chiffon*
   #:*light-blue*
   #:*light-coral*
   #:*light-cyan*
   #:*light-goldenrodyellow*
   #:*light-gray*
   #:*light-green*
   #:*light-pink*
   #:*light-salmon*
   #:*light-seagreen*
   #:*light-skyblue*
   #:*light-slategray*
   #:*light-steelblue*
   #:*light-yellow*
   #:*lime*
   #:*lime-green*
   #:*linen*
   #:*magenta*
   #:*maroon*
   #:*medium-aquamarine*
   #:*medium-blue*
   #:*medium-orchid*
   #:*medium-purple*
   #:*medium-seagreen*
   #:*medium-slateblue*
   #:*medium-springgreen*
   #:*medium-turquoise*
   #:*medium-violetred*
   #:*midnight-blue*
   #:*mint-cream*
   #:*misty-rose*
   #:*moccasin*
   #:*navajo-white*
   #:*navy*
   #:*old-lace*
   #:*olive*
   #:*olive-drab*
   #:*orange*
   #:*orange-red*
   #:*orchid*
   #:*pale-goldenrod*
   #:*pale-green*
   #:*pale-turquoise*
   #:*pale-violetred*
   #:*papaya-whip*
   #:*peach-puff*
   #:*peru*
   #:*pink*
   #:*plum*
   #:*powder-blue*
   #:*purple*
   #:*red*
   #:*rosy-brown*
   #:*royal-blue*
   #:*saddle-brown*
   #:*salmon*
   #:*sandy-brown*
   #:*sea-green*
   #:*sea-shell*
   #:*sienna*
   #:*silver*
   #:*sky-blue*
   #:*slate-blue*
   #:*slate-gray*
   #:*snow*
   #:*spring-green*
   #:*steel-blue*
   #:*tan*
   #:*teal*
   #:*thistle*
   #:*tomato*
   #:*transparent*
   #:*turquoise*
   #:*violet*
   #:*wheat*
   #:*white*
   #:*white-smoke*
   #:*yellow*
   #:*yellow-green*))

(in-package #:sol.media)

(sb-ext:add-package-local-nickname '#:colors '#:sol.media.colors)
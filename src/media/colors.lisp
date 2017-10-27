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

(in-package #:sol.media.colors)

(defparameter *alice-blue* (color :a #xff :r #xf0 :g #xf8 :b #xff))
(defparameter *antique-white* (color :a #xff :r #xfa :g #xeb :b #xd7))
(defparameter *aqua* (color :a #xff :r #x00 :g #xff :b #xff))
(defparameter *aquamarine* (color :a #xff :r #x7f :g #xff :b #xd4))
(defparameter *azure* (color :a #xff :r #xf0 :g #xff :b #xff))
(defparameter *beige* (color :a #xff :r #xf5 :g #xf5 :b #xdc))
(defparameter *bisque* (color :a #xff :r #xff :g #xe4 :b #xc4))
(defparameter *black* (color :a #xff :r #x00 :g #x00 :b #x00))
(defparameter *blanched-almond* (color :a #xff :r #xff :g #xeb :b #xcd))
(defparameter *blue* (color :a #xff :r #x00 :g #x00 :b #xff))
(defparameter *blue-violet* (color :a #xff :r #x8a :g #x2b :b #xe2))
(defparameter *brown* (color :a #xff :r #xa5 :g #x2a :b #x2a))
(defparameter *burly-wood* (color :a #xff :r #xde :g #xb8 :b #x87))
(defparameter *cadet-blue* (color :a #xff :r #x5f :g #x9e :b #xa0))
(defparameter *chartreuse* (color :a #xff :r #x7f :g #xff :b #x00))
(defparameter *chocolate* (color :a #xff :r #xd2 :g #x69 :b #x1e))
(defparameter *coral* (color :a #xff :r #xff :g #x7f :b #x50))
(defparameter *cornflower-blue* (color :a #xff :r #x64 :g #x95 :b #xed))
(defparameter *cornsilk* (color :a #xff :r #xff :g #xf8 :b #xdc))
(defparameter *crimson* (color :a #xff :r #xdc :g #x14 :b #x3c))
(defparameter *cyan* (color :a #xff :r #x00 :g #xff :b #xff))
(defparameter *dark-blue* (color :a #xff :r #x00 :g #x00 :b #x8b))
(defparameter *dark-cyan* (color :a #xff :r #x00 :g #x8b :b #x8b))
(defparameter *dark-goldenrod* (color :a #xff :r #xb8 :g #x86 :b #x0b))
(defparameter *dark-gray* (color :a #xff :r #xa9 :g #xa9 :b #xa9))
(defparameter *dark-green* (color :a #xff :r #x00 :g #x64 :b #x00))
(defparameter *dark-khaki* (color :a #xff :r #xbd :g #xb7 :b #x6b))
(defparameter *dark-magenta* (color :a #xff :r #x8b :g #x00 :b #x8b))
(defparameter *dark-olivegreen* (color :a #xff :r #x55 :g #x6b :b #x2f))
(defparameter *dark-orange* (color :a #xff :r #xff :g #x8c :b #x00))
(defparameter *dark-orchid* (color :a #xff :r #x99 :g #x32 :b #xcc))
(defparameter *dark-red* (color :a #xff :r #x8b :g #x00 :b #x00))
(defparameter *dark-salmon* (color :a #xff :r #xe9 :g #x96 :b #x7a))
(defparameter *dark-seagreen* (color :a #xff :r #x8f :g #xbc :b #x8f))
(defparameter *dark-slateblue* (color :a #xff :r #x48 :g #x3d :b #x8b))
(defparameter *dark-slategray* (color :a #xff :r #x2f :g #x4f :b #x4f))
(defparameter *dark-turquoise* (color :a #xff :r #x00 :g #xce :b #xd1))
(defparameter *dark-violet* (color :a #xff :r #x94 :g #x00 :b #xd3))
(defparameter *deep-pink* (color :a #xff :r #xff :g #x14 :b #x93))
(defparameter *deep-skyblue* (color :a #xff :r #x00 :g #xbf :b #xff))
(defparameter *dim-gray* (color :a #xff :r #x69 :g #x69 :b #x69))
(defparameter *dodger-blue* (color :a #xff :r #x1e :g #x90 :b #xff))
(defparameter *firebrick* (color :a #xff :r #xb2 :g #x22 :b #x22))
(defparameter *floral-white* (color :a #xff :r #xff :g #xfa :b #xf0))
(defparameter *forest-green* (color :a #xff :r #x22 :g #x8b :b #x22))
(defparameter *fuchsia* (color :a #xff :r #xff :g #x00 :b #xff))
(defparameter *gainsboro* (color :a #xff :r #xdc :g #xdc :b #xdc))
(defparameter *ghost-white* (color :a #xff :r #xf8 :g #xf8 :b #xff))
(defparameter *gold* (color :a #xff :r #xff :g #xd7 :b #x00))
(defparameter *goldenrod* (color :a #xff :r #xda :g #xa5 :b #x20))
(defparameter *gray* (color :a #xff :r #x80 :g #x80 :b #x80))
(defparameter *green* (color :a #xff :r #x00 :g #x80 :b #x00))
(defparameter *green-yellow* (color :a #xff :r #xad :g #xff :b #x2f))
(defparameter *honeydew* (color :a #xff :r #xf0 :g #xff :b #xf0))
(defparameter *hot-pink* (color :a #xff :r #xff :g #x69 :b #xb4))
(defparameter *indian-red* (color :a #xff :r #xcd :g #x5c :b #x5c))
(defparameter *indigo* (color :a #xff :r #x4b :g #x00 :b #x82))
(defparameter *ivory* (color :a #xff :r #xff :g #xff :b #xf0))
(defparameter *khaki* (color :a #xff :r #xf0 :g #xe6 :b #x8c))
(defparameter *lavender* (color :a #xff :r #xe6 :g #xe6 :b #xfa))
(defparameter *lavender-blush* (color :a #xff :r #xff :g #xf0 :b #xf5))
(defparameter *lawn-green* (color :a #xff :r #x7c :g #xfc :b #x00))
(defparameter *lemon-chiffon* (color :a #xff :r #xff :g #xfa :b #xcd))
(defparameter *light-blue* (color :a #xff :r #xad :g #xd8 :b #xe6))
(defparameter *light-coral* (color :a #xff :r #xf0 :g #x80 :b #x80))
(defparameter *light-cyan* (color :a #xff :r #xe0 :g #xff :b #xff))
(defparameter *light-goldenrodyellow* (color :a #xff :r #xfa :g #xfa :b #xd2))
(defparameter *light-gray* (color :a #xff :r #xd3 :g #xd3 :b #xd3))
(defparameter *light-green* (color :a #xff :r #x90 :g #xee :b #x90))
(defparameter *light-pink* (color :a #xff :r #xff :g #xb6 :b #xc1))
(defparameter *light-salmon* (color :a #xff :r #xff :g #xa0 :b #x7a))
(defparameter *light-seagreen* (color :a #xff :r #x20 :g #xb2 :b #xaa))
(defparameter *light-skyblue* (color :a #xff :r #x87 :g #xce :b #xfa))
(defparameter *light-slategray* (color :a #xff :r #x77 :g #x88 :b #x99))
(defparameter *light-steelblue* (color :a #xff :r #xb0 :g #xc4 :b #xde))
(defparameter *light-yellow* (color :a #xff :r #xff :g #xff :b #xe0))
(defparameter *lime* (color :a #xff :r #x00 :g #xff :b #x00))
(defparameter *lime-green* (color :a #xff :r #x32 :g #xcd :b #x32))
(defparameter *linen* (color :a #xff :r #xfa :g #xf0 :b #xe6))
(defparameter *magenta* (color :a #xff :r #xff :g #x00 :b #xff))
(defparameter *maroon* (color :a #xff :r #x80 :g #x00 :b #x00))
(defparameter *medium-aquamarine* (color :a #xff :r #x66 :g #xcd :b #xaa))
(defparameter *medium-blue* (color :a #xff :r #x00 :g #x00 :b #xcd))
(defparameter *medium-orchid* (color :a #xff :r #xba :g #x55 :b #xd3))
(defparameter *medium-purple* (color :a #xff :r #x93 :g #x70 :b #xdb))
(defparameter *medium-seagreen* (color :a #xff :r #x3c :g #xb3 :b #x71))
(defparameter *medium-slateblue* (color :a #xff :r #x7b :g #x68 :b #xee))
(defparameter *medium-springgreen* (color :a #xff :r #x00 :g #xfa :b #x9a))
(defparameter *medium-turquoise* (color :a #xff :r #x48 :g #xd1 :b #xcc))
(defparameter *medium-violetred* (color :a #xff :r #xc7 :g #x15 :b #x85))
(defparameter *midnight-blue* (color :a #xff :r #x19 :g #x19 :b #x70))
(defparameter *mint-cream* (color :a #xff :r #xf5 :g #xff :b #xfa))
(defparameter *misty-rose* (color :a #xff :r #xff :g #xe4 :b #xe1))
(defparameter *moccasin* (color :a #xff :r #xff :g #xe4 :b #xb5))
(defparameter *navajo-white* (color :a #xff :r #xff :g #xde :b #xad))
(defparameter *navy* (color :a #xff :r #x00 :g #x00 :b #x80))
(defparameter *old-lace* (color :a #xff :r #xfd :g #xf5 :b #xe6))
(defparameter *olive* (color :a #xff :r #x80 :g #x80 :b #x00))
(defparameter *olive-drab* (color :a #xff :r #x6b :g #x8e :b #x23))
(defparameter *orange* (color :a #xff :r #xff :g #xa5 :b #x00))
(defparameter *orange-red* (color :a #xff :r #xff :g #x45 :b #x00))
(defparameter *orchid* (color :a #xff :r #xda :g #x70 :b #xd6))
(defparameter *pale-goldenrod* (color :a #xff :r #xee :g #xe8 :b #xaa))
(defparameter *pale-green* (color :a #xff :r #x98 :g #xfb :b #x98))
(defparameter *pale-turquoise* (color :a #xff :r #xaf :g #xee :b #xee))
(defparameter *pale-violetred* (color :a #xff :r #xdb :g #x70 :b #x93))
(defparameter *papaya-whip* (color :a #xff :r #xff :g #xef :b #xd5))
(defparameter *peach-puff* (color :a #xff :r #xff :g #xda :b #xb9))
(defparameter *peru* (color :a #xff :r #xcd :g #x85 :b #x3f))
(defparameter *pink* (color :a #xff :r #xff :g #xc0 :b #xcb))
(defparameter *plum* (color :a #xff :r #xdd :g #xa0 :b #xdd))
(defparameter *powder-blue* (color :a #xff :r #xb0 :g #xe0 :b #xe6))
(defparameter *purple* (color :a #xff :r #x80 :g #x00 :b #x80))
(defparameter *red* (color :a #xff :r #xff :g #x00 :b #x00))
(defparameter *rosy-brown* (color :a #xff :r #xbc :g #x8f :b #x8f))
(defparameter *royal-blue* (color :a #xff :r #x41 :g #x69 :b #xe1))
(defparameter *saddle-brown* (color :a #xff :r #x8b :g #x45 :b #x13))
(defparameter *salmon* (color :a #xff :r #xfa :g #x80 :b #x72))
(defparameter *sandy-brown* (color :a #xff :r #xf4 :g #xa4 :b #x60))
(defparameter *sea-green* (color :a #xff :r #x2e :g #x8b :b #x57))
(defparameter *sea-shell* (color :a #xff :r #xff :g #xf5 :b #xee))
(defparameter *sienna* (color :a #xff :r #xa0 :g #x52 :b #x2d))
(defparameter *silver* (color :a #xff :r #xc0 :g #xc0 :b #xc0))
(defparameter *sky-blue* (color :a #xff :r #x87 :g #xce :b #xeb))
(defparameter *slate-blue* (color :a #xff :r #x6a :g #x5a :b #xcd))
(defparameter *slate-gray* (color :a #xff :r #x70 :g #x80 :b #x90))
(defparameter *snow* (color :a #xff :r #xff :g #xfa :b #xfa))
(defparameter *spring-green* (color :a #xff :r #x00 :g #xff :b #x7f))
(defparameter *steel-blue* (color :a #xff :r #x46 :g #x82 :b #xb4))
(defparameter *tan* (color :a #xff :r #xd2 :g #xb4 :b #x8c))
(defparameter *teal* (color :a #xff :r #x00 :g #x80 :b #x80))
(defparameter *thistle* (color :a #xff :r #xd8 :g #xbf :b #xd8))
(defparameter *tomato* (color :a #xff :r #xff :g #x63 :b #x47))
(defparameter *transparent* (color :a #x00 :r #xff :g #xff :b #xff))
(defparameter *turquoise* (color :a #xff :r #x40 :g #xe0 :b #xd0))
(defparameter *violet* (color :a #xff :r #xee :g #x82 :b #xee))
(defparameter *wheat* (color :a #xff :r #xf5 :g #xde :b #xb3))
(defparameter *white* (color :a #xff :r #xff :g #xff :b #xff))
(defparameter *white-smoke* (color :a #xff :r #xf5 :g #xf5 :b #xf5))
(defparameter *yellow* (color :a #xff :r #xff :g #xff :b #x00))
(defparameter *yellow-green* (color :a #xff :r #x9a :g #xcd :b #x32))



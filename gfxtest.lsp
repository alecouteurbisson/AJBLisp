; (load "gfxtest.lsp")

; Close window from any previous run
;(if (typep win :window)
;    (gclose win) )

; Open a new window
(setq win (gwindow 500 500 "Lisp Graphics"))

; Clear to grey
(gbrush win 0x7F7F7F :bs_solid)
(gclear win)

; Draw a grid to show graphic locations
(for (i 0 500 50)
     (gmove win 0 i)
     (gdraw win 500 i)
     (gmove win i 0)
     (gdraw win i 500) )

; Draw a box with a wide blue line
(gpen win 0xFF0000 10)
(gmove win 100 100)
(gdraw win 400 100)
(gdraw win 400 400)
(gdraw win 100 400)
(gdraw win 100 100)

; Draw a box with a dotted line
(gpen win 0x00FF00 :ps_dot)
(gmove win 200 200)
(gdraw win 300 200)
(gdraw win 300 300)
(gdraw win 200 300)
(gdraw win 200 200) 

; Draw a hyperbolic envelope of lines
(gpen win 0x0000FF :ps_solid)
(for (i 0 500 10)
     (gmove win i 0)
     (gdraw win 0 (- 500 i)) )

; Draw a hatched, dotted ellipse
(gbrush win 0xC04080 :bs_fdiag)
(gpen win 0xC0C040 :ps_dot)
(gellipse win 100 100 400 250)

; Draw a hatched, dash-dotted rectangle
(gbrush win 0xFF5080 :bs_diamond)
(gpen win 0x309080 :ps_dashdot)
(grectangle win 220 220 350 350)

; Bung some text up in Ariel
(gpen win 0xffffff :ps_solid)
(gbrush win 0x0 :bs_clear)
(println (gfont win "Ariel" 30))
(gtext win 100 400 "Text @ (100, 400)" )

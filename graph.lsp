; Open a shiny new window
; Closing any window from a previous run
(if (typep win :window)
    (gclose win) )
    
(setq win (gwindow 800 800 "Sine curve"))

; Set the viewport to something convenient
(gviewport win -0.1 -1.2 1.1 1.2)

; Draw axes...
(gmove win 0.0 -1.0)
(gdraw win 0.0 1.0)
(gmove win 0.0 0.0)
(gdraw win 1.0 0.0)

; ...and tick marks
(for (i 1 8)
     (gmove win (/ i 8.0) -0.05)
     (gdraw win (/ i 8.0)  0.05) )

; Plot a sine curve in red...
(gpen win 0x0000ff 1)
(gmove win 0.0 0.0)
(for (x 0 100)
     (gdraw win (* x 0.01) (sin (* x 0.02 :pi))) )

; ...and a cosine curve in blue
(gpen win 0xff0000 1)
(gmove win 0.0 1.0)
(for (x 0 100)
     (gdraw win (* x 0.01) (cos (* x 0.02 :pi))) )

; Write the axis legends
(gbrush win 0x0 :bs_clear)

(gfont win "Lucida Console" 15 0 :fp_fixed 0x010101)
(gtext win 1.02 0.0 "x")

(gfont win "Lucida Console" 15 0 :fp_fixed 0x0000ff)
(gtext win 0.0 1.2 "Sin(x)")

(gfont win "Lucida Console" 15 0 :fp_fixed 0xff0000)
(gtext win 0.0 1.1 "Cos(x)")

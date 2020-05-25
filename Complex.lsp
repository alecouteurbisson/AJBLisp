;; (0.0 . 0.0)  Complex
;;
(defun complex((r . 0.0) (i . 0.0)) (cons r i))
(setq re car)
(setq im cdr)
(defun mag(c) (sqrt (mag2 c)))
(defun mag2(c) (+ (* (re c) (re c)) (* (im c) (im c))))
(defun phase(c) (atan (re c) (im c)))

(defun c+ (x y)
  (complex (+ (re x) (re y)) (+ (im x) (im y))) )

(defun c- (x y)
  (complex (- (re x) (re y)) (- (im x) (im y))) )

(defun c* (x y)
  (complex (- (* (re x) (re y))
              (* (im x) (im y)) )
           (+ (* (re x) (im y))
              (* (im x) (re y)) )))

(defun mand-iter (z c) (c+ (c* z z) c))

(defun iterate (c)
  (let ((z (complex)))
    (for (i 0 64)
      (while (< (mag2 z) 4.0))
      (setq z (mand-iter z c)) )))

(defun show nil
  (let
    ((w (gwindow 400 400 "Mandy"))
     (c (complex))
     (g 0) )

    (for (r -200 200)
      (for (i -200 200)
        (setq c (complex (/ r 100.0)
                         (/ i 100.0)))
        (setq g (/ (iterate c) 15.0))
        (gpoint w (+ 200 r) (+ 200 i) (gcolour g)) ))))

(show)

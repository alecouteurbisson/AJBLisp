;; (0.0 . 0.0)  Complex
;;
(defun complex((r . 0.0) (i . 0.0)) (cons r i))
(setq re car)
(setq im cdr)
(defun mag(c) (sqrt (+ (power (re c) 2.0) (power (im c) 2.0))))
(defun mag2(c) (+ (power (re c) 2.0) (power (im c) 2.0)))
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
        (setq c (complex (- (/ r 5000.0) 0.8)
                         (- (/ i 5000.0) 0.2)))
        (setq g (/ (iterate c) 15.0))
        (gbrush w (gcolour g) :bs_solid)
        (let ((x (+ r 200))
              (y (+ i 200)) )
             (gpoint w x y) )))))

(float-format "% 10.4f")

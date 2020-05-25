(setq totals (make-vector 7 0))

(defun adjacent-bits(x)
  (let ((state t)(count 0)(result 1))
        (for (bit 6 0 -1)
             (if (eq state (btst bit x))
                 (inc count)
                 (progn
                   (if (> count result) (setq result count))
                   (setq count 0)
                   (setq state (not state)) )))
        (if (> count result) (setq result count))
        result ))

(defun adjacent-bit-counts nil
  (for (k 0 63)
       (vinc totals (adjacent-bits k)) ))

(defun vinc (v i)
  (vset v i (+ 1 (vref v i))) )

;; Need to look at distribution of bar-widths in each code
;; rather than simply the maximum bar-width

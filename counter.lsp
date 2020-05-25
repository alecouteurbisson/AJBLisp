;;; Counter synthesis
(defun truth-table(seq bit)
  (let ((prior (last seq))
        (tt nil))
    (for-each (state seq)
      (when (btst bit state)
            (push prior tt))
      (setq prior state) )
    tt ))

(defun minterm> (a b)
  (> (bcnt a) (bcnt b)) )

(defun sort-terms(tt)
  (sort tt minterm>))

(defun bin(x n)
  (let ((ch (make-vector n #\0)))
    (for (bit 0 (- n 1))
      (when (btst bit x)
            (vset ch bit #\1)))
    (string (reverse ch)) ))

(defun show (bit)
  (let ((tt (truth-table seq bit)))
    (sort-terms tt)
    (for-each (minterm tt)
      (println (bin minterm 8)) )))

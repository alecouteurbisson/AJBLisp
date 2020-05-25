;; Priority queue 
;;

;; A heap in a vector
(setq h (make-vector 100))
(vset h 0 1)

;; Branches
(defun L (n) (+ n n))
(defun R (n) (+ n n 1))
(defun P (n) (bshift n -1))

;; Insert
(defun hinsert (h v (at))
  (unless at
         (setq at (vref h 0)) ; get free node
         (vset h 0 (+ at 1)))
  (slet ((pn (P at)) (pv (vref h pn)))
    (if (or (zerop pn)
            (<= pv v))
        (vset h at v)
        (progn
          (vset h at pv)
          (hinsert h v pn) ))))
          
;; Delete
(defun hdelete (h)
  (slet ((at 1)
         (child)
         (size (- (vref h 0) 1))
         (result (vref h 1))
         (v (vref h size)))
    (vset h 0 size)
    (if (zerop size)
        nil
        (loop
          (cond 
            ((and (< (setq child (L at)) size)
                  (> v (vref h child))
                  (< (vref h child) (vref h (R at))) )
             (vset h at (vref h child))
             (setq at child) )
            ((and (< (setq child (R at)) size)
                  (> v (vref h child)))
             (vset h at (vref h child))
             (setq at child) )
            (t
             (vset h at v)
             (break result) ))))))
               


;; Shuffle a list l by reordering elements randomly
;; k times
(defun shuffle (l k)
  (for (i 1 k)
    (slet ((n (random (- (length l) 1)))
           (elem (nthcdr (+ n 1) l)) )
          (rplacd (nthcdr n l) (cdr elem))
          (setq l (rplacd elem l)) ))
  l )

(defun hprint (h)
  (for (i 1 (- (vref h 0) 1))
       (print (vref h i) :space) ))

(setq items nil)

(for (i 1 30) (push i items))

(setq items (shuffle items 50))

(for-each (x items) (hinsert h x))

(loop
  (print (while (hdelete h)) :tab)
  (hprint h)
  (println) )

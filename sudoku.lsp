(setq grid1
#(0 0 8   0 9 0   0 5 6
  0 0 0   0 0 0   0 2 0
  9 0 0   5 0 3   1 0 0

  0 1 0   0 7 8   6 0 5
  0 0 0   0 0 0   0 0 0
  6 0 5   1 4 0   0 8 0

  0 0 1   6 0 7   0 0 3
  0 2 0   0 0 0   0 0 0
  4 6 0   0 1 0   5 0 0) )

(setq grid2
#(0 0 2   3 0 0   7 0 0
  0 0 4   0 0 9   0 0 0
  6 0 0   0 0 0   0 5 0

  0 7 0   0 0 2   0 6 0
  0 0 3   7 0 0   4 0 0
  0 1 0   0 0 0   0 2 0

  0 3 0   0 0 0   0 0 9
  0 0 0   4 0 0   6 0 0
  0 0 5   0 0 8   2 0 0))


(setq grid3  ; difficult
#(3 0 6   8 0 0   0 9 2
  8 0 0   0 0 5   0 0 0
  0 0 0   2 9 0   0 0 3

  0 4 0   0 0 0   9 0 6
  0 0 2   0 0 0   7 0 0
  5 0 3   0 0 0   0 8 0

  6 0 0   0 1 9   0 0 0
  0 0 0   3 0 0   0 0 9
  7 9 0   0 0 6   3 0 5) )

(setq grid4  ; mild
#(3 0 5   0 0 0   7 0 1
  1 7 0   0 0 3   0 6 4
  0 0 0   0 0 1   8 0 0

  2 0 1   0 6 0   0 0 7
  0 0 0   1 0 7   0 0 0
  7 0 0   0 2 0   9 0 3

  0 0 3   7 0 0   0 0 0
  9 4 0   6 0 0   0 5 2
  6 0 8   0 0 0   3 0 9) )

(setq grid5  ; fiendish
#(0 0 4   7 0 2   0 8 0
  3 5 0   0 0 9   0 6 0
  0 0 0   0 0 0   0 0 1

  8 9 0   5 0 6   0 0 7
  0 0 0   0 0 0   0 0 0
  6 0 0   3 0 4   0 5 9

  7 0 0   0 0 0   0 0 0
  0 8 0   2 0 0   0 1 6
  0 2 0   6 0 5   4 0 0) )



;; grid mapping functions

; grid index to column axis
(defun g2c(g)
  (rem g 9) )

; grid index to row axis
(defun g2r(g)
  (/ g 9) )

; grid index to box axis
(defun g2b(g)
  (+ (* 3 (/ g 27))
     (rem (/ g 3) 3)) )

;; Base and offset of box axes (added to give grid position)
(defconstant bb #(0 3 6 27 30 33 54 57 60))
(defconstant bo #(0 1 2 9 10 11 18 19 20))

;; get placed values on same row column or box
(defun colocated(g)
 (let ((r (* 9 (g2r g)))      ; base of row
       (c (g2c g))            ; base of col
       (b (vref bb (g2b g)))  ; base of block
       (co) )
       (for (i 0 8)
         (push (vref grid (+ i r)) co)
         (push (vref grid (+ (* 9 i) c)) co)
         (push (vref grid (+ b (vref bo i))) co) )
       ;(println g " co = " co)
       (sort-uniq co) ))

; identity function
(defun id(g) g)

; grid index to placed value
; spaces map to -1 which display as '.'
(defun g2v(g)
  (let ((v (vref grid g)))
       (if (zerop v) -1 v) ))

;; Display the grid
(defun show-grid (f)
  (for (g 0 80)
       (let ((v (f g)))
            (when (and (<> g 0) (zerop (rem g 27)))
              (println " ------+-------+------") )
            (when (or(= 6 (rem g 9)) (= 3 (rem g 9)))
              (print " |"))
            (if (plusp v)
              (print-format " %1d" v)
              (print " .") ))

            (when (= 8 (rem g 9))
              (println) ))
  (println) )

;; Produce the distinct, non-zero elements of l (which is always numeric)
(defun sort-uniq(l)
  (let ((v (vector l))
        (last 0)                ; This eliminates zeros by matching initial zeros in v
        (result))
       (vsort v)
       (for-each (el v)          ; Accumulate unique elements in result
         (unless (= el last)
                 (push el result)
                 (setq last el) ))
       result ))

;; Invert a set of digit values
;; e.g. (1 2 3 5 9) => (4 6 7 8)
(defun invert(l)
  (let ((inv))
     (for (i 1 9)
        (unless (member i l)
                (push i inv) ))
     inv ))

;; Brute force solver
(defkey :done)

(defun solve-next(g)
  (when (= 81 (inc g))
        (throw :done) )

  ; If digit is missing
  (if (zerop (vref grid g))
      (let ((pd (invert (colocated g))))
           (when pd
               (for-each (digit pd)
                 (vset grid g digit)
                 (solve-next g)
                 (vset grid g 0) )))
      (solve-next g) ))

;; Call the solver and catch the exit
(defun solve()
  (catch :done
    (solve-next -1) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Solve
(defun solve-grid(lit-grid)
  (setq grid (vector lit-grid))
  (show-grid g2v)
  (collect)
  (statistics 0)
  (push (timer (solve)) times)
  (println (car times) "mS " (statistics))
  (show-grid g2v)
  (println)
  (println)
  (println) )

(setq times nil)

(solve-grid grid1)
(solve-grid grid2)
(solve-grid grid3)
(solve-grid grid4)
(solve-grid grid5)

(println times " -> " (apply + times))

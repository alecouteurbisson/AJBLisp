;;; SECD machine simulator in AJBLisp

(defkey :omega)

(setq S nil
      E nil
      C nil
      D nil)

(defun $nil()
  (push nil S))

(defun $ldc()
  (push (pop C) S))

(defun $ld()
   (let ((frm (nth (caar C) E)))
        (for (k (cdar C) 1 -1) (pop frm))
        (push (if (atom frm) frm (car frm)) S)
        (pop C) ))

(defun $ldf()
  (push (cons (pop C) E) S) )

(defun $ap()
   (push C D)
   (push E D)
   (setq E (pop S))
   (setq C (pop E))
   (push (pop S) E)
   (push S D)
   (setq S nil) )

(defun $rtn()
  (setq S (cons (car S) (pop D))
        E (pop D)
        C (pop D) ))

(defun $dum()
  (push :omega E) )

(defun $rap()
  (push C D)
  (push (cdr E) D)
  (setq E (pop S))
  (setq C (pop E))
  (rplaca E (pop S))
  (push S D)
  (setq S nil) )

(defun $sel()
  (let ((ct (pop C))
        (cf (pop C))
        (p  (pop S)) )
       (push C D)
       (setq C (if (zerop p) cf ct)) ))

(defun $join()
  (setq C (pop D)) )

(defun $eq()
  (if (eq (pop S) (pop S))
      (push 1 S)
      (push 0 S) ))

(defun $gt()
  (if (> (pop S) (pop S))
      (push 0 S)
      (push 1 S) ))

(defun $add()
  (push (+ (pop S) (pop S)) S) )

(defun $sub()
  (push (+ (- (pop S)) (pop S)) S) )

(defun $mul()
  (push (* (pop S) (pop S)) S) )

(defun $div()
  (push (* (/ (pop S)) (pop S)) S) )

(defun $atom()
  (push (atom (pop S)) S))

(defun $cons()
  (let ((d (pop S))
        (a (pop S)) )
       (push (cons d a) S) ))

(defun $car()
  (push (car (pop S)) S))

(defun $cdr()
  (push (cdr (pop S)) S))

(defun $stop()
  (break "stopped"))

(defun vm-run(code)
  (setq C code
        S nil
        E nil
        D nil)
  (loop
    ;(println "S = " S)
    ;(println "E = " E)
    ;(println "C = " C)
    ;(println "D = " D)

    (println "C = " C "\t")
;    (if (eq (car C) '$sel)
;      (println (nth 1 C) "  " (nth 2 C))
;      (if (< (length C) 2)
;          (println)
;          (if (and (> (length C) 1)
;                   (atom (cadr C))
;                   (eql #\$ (char (string (cadr C)) 0)))
;              (println)
;              (println (cadr C)) )))


    (eval (list (pop C))) )
  (println)
  (println "Result = " (pop S))
  (println)  )

; (run '($ldc 5 $atom $sel ($ldc 9 $join) ($ldc 7 $join) $stop))

; (run '($ldc 5 $ldc 3 $cons
;        $ldf ($ld (0 . 1) $ld (0 . 0) $add $rtn)
;        $ap $stop) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is loosley based upon the pretty printer algorithm
;; from Xlisp originally written by Gregory Frascadore.
;; I have included his original copyright message here.
;; Xlisp is a Scheme interpreter written by David Betz.
;;
;; I have modified the algorithm to print a space before the first
;; unmatched ')' on a line (i.e. as this program is formatted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               PP 1.0 : (C) Copyright 1985 by Gregory Frascadore
;;
;;   This software may be copied, modified, and distributed to others as long
;;   as it is not sold for profit, and as long as this copyright notice is
;;   retained intact. For further information contact the author at:
;;               frascado%umn-cs.CSNET   (on CSNET)
;;               75106,662               (on CompuServe)


;; Create the global data required
(setq :pp-width 0           ; Width allowed before breaking
      :pp-out   nil         ; Output destination
      :pp-xpos  0           ; Current cursor position
      :pp-lpar  nil         ; Stack of list opening points
      :pp-rpar  nil         ; Count of closing parens
      :pp-spc   nil         ; Stack of space locations in closing parens
      :pp-pspc  t)          ; Flag set when spaces to be printed in closing parens

;; Main entry point
(defun pprint (expr (out . :stdout) (width . 50))
  (setq :pp-width  width
        :pp-out    out
        :pp-xpos   0
        :pp-lpar   '(0)
        :pp-rpar   0
        :pp-spc    nil)
  (:pp-expr expr)
  (println) )

(defun :pp-expr (expr)
  (cond
    ((consp expr)
     (:pp-list expr) )
    ((vectorp expr)
     (:pp-vector expr) )
    (t
     (:pp-write expr) )))

;; Print a list
(defun :pp-list(expr)
  (cond
;; Works better without width
;    ((< (write-size expr) :pp-width)
;     (:pp-write expr) )

    ((atom (car expr))
      (cond
        ;; special treatment for function definitions
        ((member (car expr) '(defun defmacro lambda macro))
         (push (+ :pp-xpos 2) :pp-lpar)
         (:pp-start)
         (:pp-write (pop expr))
         (:pp-print " ")
         (:pp-write (pop expr))
         (:pp-newline) )
        ;; special treatment for (p)setq
        ((member (car expr) '(setq psetq))
         (:pp-start)
         (:pp-write (pop expr))
         (:pp-print " ")
         (push :pp-xpos :pp-lpar)
         (let ((flip t))
           (for-each (x expr)
             (if (setq flip (not flip))
                 (progn
                   (:pp-print :space)
                   (:pp-write (pop expr))
                   (when expr (:pp-newline)) )
                 (when expr (:pp-expr (pop expr)) ))))
         (pop :pp-lpar))
         ;; print (quote x) as 'x
        ((eq 'quote (car expr))
         (:pp-print "'")
         (pop expr)
         (:pp-start)
         (:pp-expr (pop expr)))
        (t
         (:pp-start)
         (:pp-write (pop expr)) ))
      (:pp-print " ")
      (push :pp-xpos :pp-lpar)
      (:pp-rest expr)
      (pop :pp-lpar)
      (:pp-finish) )
    (t
      (:pp-start)
      (push :pp-xpos :pp-lpar)
      (:pp-rest expr)
      (pop :pp-lpar)
      (:pp-finish) )))

(defun :pp-start ()
  (:pp-print "(")
  (inc :pp-rpar) )

(defun :pp-rest (rest)
  (loop
    (while rest)
    (until (atom rest)
           (print :pp-out " . ")
           (:pp-expr (pop rest)) )
    (:pp-expr (pop rest))
    (when rest (:pp-newline)) ))

(defun :pp-finish ()
  (when (and :pp-spc (= (car :pp-spc) :pp-rpar))
    ;; print only one closing space between newlines
    (when :pp-pspc
          (:pp-print " ")
          (setq :pp-pspc nil) )
    (pop :pp-spc) )
  (:pp-print ")")
  (dec :pp-rpar) )

(defun :pp-newline ()
    (push :pp-rpar :pp-spc)
  (println :pp-out)
  ;; allow one more closing space
  (setq :pp-pspc t)
  (:pp-spaces (car :pp-lpar))
  (setq :pp-xpos (car :pp-lpar)) )

(defun :pp-write (expr)
  (inc :pp-xpos (write-size expr))
  (write :pp-out expr) )

(defun :pp-print (expr)
  (inc :pp-xpos (print-size expr))
  (print :pp-out expr) )

(defun :pp-spaces (n)
  (for (k 1 n) (print :pp-out " ")) )

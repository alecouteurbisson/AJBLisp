;;Code from PLisp's Basics.lsp.
;;
;;
;; (umdr '(0 1 2 3 4 5 6 7 8 9)) reverses the list to (9 8 7 6 5 4 3 2 1 0)	
;; due to its very inefficient algorithm it is a nice benchmark for basic
;; lisp functions like car, cdr cons
;; On my machine (P166MMX,64MB Ram, 300000 allocated nodes)
;; umdr takes 17 sec. in interpreted mode.
;; For comparison see results from the literature:
;;    (Frank Spade, KI-Rundbrief 38, Gesellschaft fuer Informatik)
;; Symbolics 3670: 61 sec.
;; Interlisp (Siemens 7.5500): 55 sec.
;; Xerox 1108: 165 sec.
;;
;; But... under AJBLisp on a 300MHz PII it takes around 1 sec!
;; I was quite surprised at the speed difference to say the least.
;;	
(defun umdr (lst)
  (if (null lst)
      nil
      (if (null (cdr lst))
          lst
          (cons (car (umdr (cdr lst)))
                (umdr (cons (car lst)
                            (umdr (cdr (umdr (cdr lst)))) ))))))

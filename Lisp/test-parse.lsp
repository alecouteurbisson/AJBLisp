;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A recursive descent parser for algebraic expressions  ;;
;; AJB  - August 1997                                    ;;
;;                                                       ;;
;; This version modified for test purposes               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This parser is designed to produce results that are
; easily processed:
; Sums and products are collected into a single, varadic
; operation.
; Subtraction and division are treated as the sum of the
; negative and the product of the inverse respectivly
; Therefore "a+b-c+d" is parsed as
;	(+ a b (- c) d)
; and not as
;	(+ a (+ b (+ (- c) d)))
;
; Note that the division operator is only used in its
; unary form, (/ x), which returns the 1/x
;
; The allowed operators are:
; operator operation     location associativity precedence
; +        addition      infix    left          10
; -        subtraction   infix    left          10
; *        mutiplication infix    left          20
; /        division      infix    left          20
; +        null          prefix   right         40
; -        negation      prefix   right         40
; ^        power         infix    right         50
; !        factorial     postfix  left          60
; f()      function      matchfix
; ()       parentheses   matchfix

; Read a token from expr, checking for tokens put back
(defun get-token ()
  (cond ((null tokens) (token expr))
        (t (prog1 (setq tok (car tokens))
                  (setq tokens (cdr tokens)) ))))

; Put a token back
(defun put-back (tok)
  (setq tokens (cons tok tokens)) )


; Parse a string expression
(defun parse (s)
  (if (not (stringp s)) (error 1  s "Parse expects a string argument"))
  (setq tokens (tokenise s))
  (parse-expr) )

; Parse a sum of products
(defun parse-expr ()
  (let ((sum (list (parse-product))) (rhs) (tok))
       (loop
         (while (setq tok (pop tokens)) lhs)
         (until (eql tok :rpar))
         (cond
           ((eql tok #\+)
            (setq rhs (parse-product))
            (if (and (listp rhs) (eq '+ (car rhs)))
                (setq sum (append sum (cdr rhs)))
                (setq sum (append sum (list rhs))) ))
           ((eql tok #\-)
            (setq rhs (parse-product))
            (if (and (listp rhs) (eq '+ (car rhs)))
                (setq sum (append sum (negate (cdr rhs))))
                (setq sum (append sum (list (list '- rhs)))) ))
           (t (error 1 tok "Bad token")) ))
       (if (cdr sum)
         (cons '+ sum)
         (car sum) )))

; Parse a product of factors
(defun parse-product ()
  (let ((prd (list (parse-other))) (rhs) (tok))
       (loop
         (while (setq tok (pop tokens)))
         (cond
           ((eql tok #\*)
            (setq rhs (parse-other))
            (if (and (listp rhs) (eq '* (car rhs)))
                (setq prd (append prd (cdr rhs)))
                (setq prd (append prd (list rhs))) ))
           ((eql tok #\/)
            (setq rhs (parse-other))
            (if (and (listp rhs) (eq '* (car rhs)))
                (setq prd (append prd (inverse (cdr rhs))))
                (setq prd (append prd (list (list '/ rhs)))) ))
           (t (push tok tokens) (while nil)) ))
       (if (cdr prd)
         (cons '* prd)
         (car prd) )))

; Parse other operators
(defun parse-other ()
  (let ((lhs (parse-term))
        (tok (pop tokens)) )
       ; Factorial?
       (cond
         ((eql tok #\!)
          (setq lhs (list 'fact lhs))
          (setq tok (get-token))
           lhs )
         ((eql tok #\^^)
          (list 'power lhs (parse-other)) )
         (t
           (push tok tokens)
           lhs ))))

; Parse a term
(defun parse-term ()
  (let ((lhs (pop tokens)))
       (cond
         ; A function?
         ((member lhs functions)
          (if (eql (pop tokens) :lpar)
              (list lhs (parse-expr))
              (error 1 lhs "Function missing argument list") ))
         ; Unary +?
         ((eql lhs #\+) (parse-other))
         ; Unary -?
         ((eql lhs #\-) (list '- (parse-other)))
         ; A sub expression?
         ((eql lhs :lpar) (parse-expr))
         ; Just a number or a symbol
         ((eq lhs 'e) :e)
         ((eq lhs 'pi) :pi)
         ((or (symbolp lhs) (numberp lhs)) lhs)
         (t (error 1 lhs "Parse error")) )))

; Known functions, extend as desired
(setq functions '(sqrt sin cos tan asin acos atan log ln exp
                  sinh cosh tanh asinh acosh atanh random))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression output functions

(defun print-expr (e (prec . 0))
  (cond
    ((null e))
    ((atom e)                   (print e))
    ((eq (car e) '+)            (print-sum (cdr e) prec))      ; prec = 10
    ((eq (car e) '*)            (print-product (cdr e) prec))  ; prec = 20
    ((eq (car e) '/)            (print-inverse (cdr e) prec))  ; prec = 30
    ((eq (car e) '-)            (print-neg (cdr e) prec))      ; prec = 40
    ((eq (car e) 'power)        (print-power (cdr e) prec))    ; prec = 50
    ((eq (car e) 'fact)         (print-fact (cdr e)))          ; prec = 60
    ((member (car e) functions) (print-func e))                ; prec = infinity
    (t (error 1 e "Unknown expression type in print-expr")) ))

(defun print-sum (e (prec . 0))
  (if (> prec 10) (print :lpar))
  (print-expr (car e) 10)
  (loop
    (setq e (cdr e))
    (while e)
    (cond
      ((and (listp (car e)) (eq (caar e) '-))
        (print " - ")
        (print-expr (car (cdar e)) 10) )
      (t
        (print " + ")
        (print-expr (car e) 10) )))
  (if (> prec 10) (print :rpar)) )

(defun print-product (e (prec . 0))
  (if (> prec 20) (print :lpar))
  (print-expr (car e) 20)
  (loop
    (setq e (cdr e))
    (while e)
    (cond
      ((and (listp (car e)) (eq (caar e) '/))
        (print " / ")
        (print-expr (car (cdar e)) 20) )
      (t
        (print " * ")
        (print-expr (car e) 20) )))
   (if (> prec 20) (print :rpar)) )

 (defun print-inverse(e (prec . 0))
   (if (> prec 30) (print :lpar))
   (print "/")
   (print-expr (car e) 30)
   (if (> prec 30) (print :rpar)) )

 (defun print-neg(e (prec . 0))
   (if (> prec 40) (print :lpar))
   (print "-")
   (print-expr (car e) 40)
   (if (> prec 40) (print :rpar)) )

 (defun print-power(e (prec . 0))
   (if (> prec 50) (print :lpar))
   (print-expr (car e) 50)
   (print " ^ ")
   (print-expr (cadr e) 50)
   (if (> prec 50) (print :rpar)) )

 (defun print-fact(e (prec . 0))
   (if (> prec 60) (print :lpar))
   (print-expr (car e) 60)
   (print "!")
   (if (> prec 60) (print :rpar)) )

 (defun print-func(e (prec . 0))
   (print (car e))
   (print :lpar)
   (print-expr (cadr e))
   (print :rpar) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

; Negate an atom or list
(defun negate (l)
  (cond
    ((null l) nil)
    ((atom l) (cons '- (list l)))
    (t (cons (negate (car l)) (negate (cdr l)))) ))

; Inverse of an atom or list
(defun inverse (l)
  (cond
    ((null l) nil)
    ((atom l) (cons '/ (list l)))
    (t (cons (inverse (car l)) (inverse (cdr l)))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the parser

; Test expressions
(setq expressions
  '( "a+b"
     "a-b"
     "a*b"
     "a/b"
     "a+b-c+d"
     "a*b/c*d"
     "120"
     "120.0"
     "-240.324E-6"
     "12*a+4*b"
     "sin(2*pi)-tan(32*2*pi/180)"
     "(a+b) / (a-b)"
     "exp(ln(x)*3)"
     "a^b^c"
     "-x^2-b*x-c" ))

; Parse a list of expressions
(defun test-parse (tests (verbose))
  (let ((e))
       (loop
         (while tests nil)
         (if verbose
           (progn
             (println "****************************************"
                      "***************************************")
             (println   " expr   : " (car tests))
             (println " parse  : " (setq e (parse (car tests)))) )
            (progn
             (setq e (parse (car tests)))))
         (setq tests (cdr tests)) )))



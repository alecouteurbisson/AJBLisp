;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A recursive descent parser for algebraic expressions  ;;
;; AJB  - August 1997                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This parser is designed to produce results that are
; easily processed:
; Sums and products are collected into a single, varadic
; operations.
; Subtraction and division are treated as the sum of the
; negative and the product of the inverse respectivly
; Therefore "a+b-c+d" is parsed as
;	(+ a b (- c) d)
; and not as
;	(+ a (+ b (+ (- c) d)))
;
; Note that the subtraction and division operators are only
; used in their unary forms in the parsed expressions.
; i.e. (- x) returning -x and (/ x), returning 1/x.
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

; Parse a string expression
(defun parse (s)
  (unless (stringp s) (error 1  s "Parse expects a string argument"))
  (setq tokens (tokenise s))
  (parse-expr) )

; Parse a sum of products
(defun parse-expr ()
  (let ((sum (list (parse-product))) (rhs) (tok))
       (loop
         (while (setq tok (pop tokens)) sum)
         (until (eql tok ")") sum)
         (cond
           ((eql tok "+")
            (setq rhs (parse-product))
            (if (and (listp rhs) (eq '+ (car rhs)))
                (setq sum (append sum (cdr rhs)))
                (setq sum (append sum (list rhs))) ))
           ((eql tok "-")
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
           ((eql tok "*")
            (setq rhs (parse-other))
            (if (and (listp rhs) (eq '* (car rhs)))
                (setq prd (append prd (cdr rhs)))
                (setq prd (append prd (list rhs))) ))
           ((eql tok "/")
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
         ((eql tok "!")
          (setq lhs (list 'fact lhs))
          (setq tok (pop tokens))
           lhs )
         ((eql tok "^")
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
          (if (eql (pop tokens) "(")
              (list lhs (parse-expr))
              (error 1 lhs "Function missing argument list") ))
         ; Unary +?
         ((eql lhs "+") (parse-other))
         ; Unary -?
         ((eql lhs "-") (list '- (parse-other)))
         ; A sub expression?
         ((eql lhs "(") (parse-expr))
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
    ((atom e)                   (princ e))
    ((eq (car e) '+)            (print-sum (cdr e) prec))      ; prec = 10
    ((eq (car e) '*)            (print-product (cdr e) prec))  ; prec = 20
    ((eq (car e) '/)            (print-inverse (cdr e) prec))  ; prec = 30
    ((eq (car e) '-)            (print-neg (cdr e) prec))      ; prec = 40
    ((eq (car e) 'power)        (print-power (cdr e) prec))    ; prec = 50
    ((eq (car e) 'fact)         (print-fact (cdr e)))          ; prec = 60
    ((member (car e) functions) (print-func e))                ; prec = infinity
    (t (error 1 e "Unknown expression type in print-expr")) ))

(defun print-sum (e (prec . 0))
  (when (> prec 10) (princ lpar))
  (print-expr (car e) 10)
  (loop
    (pop e)
    (while e)
    (cond
      ((and (listp (car e)) (eq (caar e) '-))
        (princ " - ")
        (print-expr (car (cdar e)) 10) )
      (t
        (princ " + ")
        (print-expr (car e) 10) )))
  (when (> prec 10) (princ rpar)) )

(defun print-product (e (prec . 0))
  (when (> prec 20) (princ lpar))
  (print-expr (car e) 20)
  (loop
    (pop e)
    (while e)
    (cond
      ((and (listp (car e)) (eq (caar e) '/))
        (princ " / ")
        (print-expr (car (cdar e)) 20) )
      (t
        (princ " * ")
        (print-expr (car e) 20) )))
   (when (> prec 20) (princ rpar)) )

 (defun print-inverse(e (prec . 0))
   (when (> prec 30) (princ lpar))
   (princ "/")
   (print-expr (car e) 30)
   (when (> prec 30) (princ rpar)) )

 (defun print-neg(e (prec . 0))
   (when (> prec 40) (princ lpar))
   (princ "-")
   (print-expr (car e) 40)
   (when (> prec 40) (princ rpar)) )

 (defun print-power(e (prec . 0))
   (when (> prec 50) (princ lpar))
   (print-expr (car e) 50)
   (princ " ^ ")
   (print-expr (cadr e) 50)
   (when (> prec 50) (princ rpar)) )

 (defun print-fact(e (prec . 0))
   (when (> prec 60) (princ lpar))
   (print-expr (car e) 60)
   (princ "!")
   (when (> prec 60) (princ rpar)) )

 (defun print-func(e (prec . 0))
   (princ (car e))
   (princ lpar)
   (print-expr (cadr e))
   (princ rpar) )


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

(defconstant lpar '\()
(defconstant rpar '\))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the parser

; REP loop
(defun parser ()
  (let ((expr)(val))
    (loop
      (flush)
      (princ "\nexpression: ")
      (setq expr (readline))
      (while (not (eql expr "")) "done")
      (printc "result:     " (setq val (parse expr)))
      (setq val (errorset (eval val)))
      (if (listp val)
          (printc "value:      " (car val)) ))))

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
     "-x^2-b*x-c"
     "k!/(n-k)!" ))

; Parse a list of expressions
(defun test-parse (tests)
  (let ((e))
    (loop
      (while tests nil)
      (printc "****************************************"
              "****************************************" )
      (printc " expr   : " (car tests))
      (printc " parse  : " (setq e (parse (car tests))))
      (princ  " deparse: ")
      (print-expr e)
      (print)
      (pop tests) )))



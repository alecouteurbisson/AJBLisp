;(load "transform.lsp" t)
;(let ((a (geta))
;      (b (getb)) )
;     (f a b))
;
;((lambda (a b) (f a b)) (geta) (getb))



;(print (cons a b) (eval x))

;(lambda (tmp1 tmp2) (print tmp1 tmp2) (cons a b) (eval x))

(defun name-vars (expr)
  (cond
    ((atom expr)
     expr)

    ((eq (car expr) lambda)
     (cons lambda (cons (cadr expr) (name-vars (cddr expr)))) )

    ((eq (car expr) 'let)
     (let ((frm (vars (cadr expr))))
          (cons (list lambda
                      frm
                      (name-vars (car (cddr expr))) )
                (exprs (cadr expr)) )))
    (t
      (let ((frm))
           (if
             (eval (cons and (mapc atom (cdr expr))))
             expr
             (progn
               (for-each (f (cdr expr))
                 (push (gensym "tmp") frm) )
               (cons (list lambda
                           frm
                           (cons (name-vars (car expr)) frm))
                     (name-list (cdr expr)) )))))))

(defun name-list(l)
  (mapc name-vars l))

(defun vars (d)
  (if (eq d nil) nil
      (cons (car (car d)) (vars (cdr d)))))

(defun exprs (d)
  (if (eq d nil) nil
      (cons (cadr (car d)) (exprs (cdr d)))))

(defun curry(expr)
  (cond
   ((atom expr)
    expr)

   ((and (consp (car expr)) (eq (caar expr) lambda))
    (curry-lambda (car (cdar expr)) (cdr (cdar expr)) (cdr expr)) )

   (t
    (cons (curry (car expr)) (mapc curry (cdr expr))) )))

(defun curry-lambda (frm  body args)
  (if (null frm)
      (curry body)
      (if (null (cdr frm))
          (list (cons lambda (cons (list (car frm)) (curry  body)))
                (car args) )
          (list (cons lambda
                      (cons (list (car frm))
                            (curry-lambda (cdr frm) body (cdr args))))
                (car args) ))))

(pprint (name-vars '(print (cons a b) (eval x)) ))

(setq e1 '(let ((a (geta))
                (b (getb)) )
               (f a b)) )

(setq e2 '(let ((a (geta))
                (b (getb)) )
               (f (car a) b)) )

(pprint (curry (name-vars e1)))
(println)

(pprint (curry (name-vars e2)))
(println)

(pprint (curry '((lambda (tmp1 tmp2) (print tmp1 tmp2)) (cons a b) (eval x))))
(println)


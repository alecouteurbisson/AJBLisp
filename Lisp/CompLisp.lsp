(setq *env* nil)

(setq expr
  '(lambda (x) (if (< 2 x) 1 (* x (fac (- x 1))))) )

(defun beta-convert(expr)
  (cond
    ((null expr) nil)

    ((symbolp expr)(ref expr))

    ((atom expr) expr)

    ((lambdap expr)
     (let
       ((oldenv *env*) (nf))
       (pop expr)
       (for-each (f (pop expr))
         (push (gensym (string f)) nf)
         (push (cons f (car nf)) *env*) )
       (prog1
         (cons lambda (cons (reverse nf) (mapc beta-convert expr)))
         (setq *env* oldenv) )))

    (t (cons (beta-convert (car expr))
             (beta-convert (cdr expr)) ))))

(defun cps(cont expr)
  (writeln "(cps " cont expr ")")
  (cond
    ((atom expr) (list cont expr))
    ((lambdap expr)
     (list lambda
           (cons 'cont (cadr expr))
           (cps-progn 'cont (cddr expr)) ))
    ((eq (car expr) 'if)
     (list (pop expr)
           (pop expr)
           (cps cont (pop expr))
           (cps cont (if (null expr) nil (car expr))) ))
    (t
      (cons (car expr) (cps-list cont (cdr expr))) )))

(defun cps-list(cont l)
  (cond
    ((atom l) (list cont l))
    (t
      (cons (cps cont (car l)) (cps-list cont (cdr l))))))

(defun cps-progn (cont exprs)
  (cond
    ((null (cdr exprs)) (cps cont (car exprs)))
    (t (list (car exprs) (cps-progn cont (cdr exprs)))) ))

(defun ref(sym)
  (let ((r (assoc sym *env*)))
       (if r (cdr r) sym) ))

(defun lambdap(x)
  (and (consp x) (eq (car x) lambda)) )

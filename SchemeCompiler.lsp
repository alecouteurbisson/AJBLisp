(defun compile(expr env)
  (cond
    ((null expr)
     nil )

    ((symbolp expr)
     (compile-symbol expr env) )

    ((atom expr)
     expr )

    (t
     (compile-function (car expr) (cdr expr) env) )))

(defun compile-symbol(sym env)
  (let ((renamed (cdrassoc sym env)))
       (if (eq :undefined renamed)
           sym
           renamed )))

(defun compile-function(func args env)
  (cond
    ((symbolp func) (compile-named-function func args env))
    ((lambdap func) (compile-lambda-app (cdr func) args env))
    (t
     (error 1 func "Illegal function") )))

(defun compile-lambda-app(func args env)
  (cons
    (compile-lambda (car func) (cdr func) (extend-env (car func) env))
    (compile-list args env) ))

(defun compile-lambda(args body env)
  (list 'lambda (compile-list args env) (compile (progn-list body) env)) )

(defun compile-named-function(func args env)
  (case func
    ((car cdr not)
     (if (<> (length args) 1)
         (error 1 args "Incorrect number of args")
         (cons func (compile-list args env)) ))

    ((cons  eq )
     (if (<> (length args) 2)
         (error 1 args "Incorrect number of args")
         (cons func (compile-list args env)) ))

    (progn
     (cons func (compile-list args env)) )

    (quote (println 'quote args)
      (if (lambdap (car args))         ; Not nice!
          (cons func
                (list
                  (compile-lambda
                    (car (cdr (car args)))
                    (cdr (cdr (car args)))
                    (extend-env (car (cdr (car args))) env) )))
          (cons func args) ))

    ((+ - * and or)
     (compile-binop func args env) )

    (let
     (compile (rewrite-let args) env) )

    (cond
     (compile-cond args env) )

    (lambda
      (compile-lambda (car args) (cdr args) env) )

    (:else
     (if (symbolp func)
         (cons (compile-symbol func env) (compile-list args env))
         (error 1 func "Illegal function") ))))

(defun rewrite-let(expr)
  (let ((bindings (car expr))
        (body (progn-list (cdr expr))) )
       (append (list (list 'lambda
                     (targets bindings)
                     body ))
               (compile-list (values bindings) env) )))

(defun compile-cond(args env)
  (cond
    ((null args)
     nil )
    ((not (consp args))
     (error 1 args "Illegal cond clause") )
    ((eq (car (car args)) t)
     (compile (progn-list (cdr (car args))) env) )
    (t
     (list 'if
           (compile (car (car args)) env)
           (compile (progn-list (cdr (car args))) env)
           (compile-cond (cdr args) env) ))))

(defun compile-binop(func args env)
  (if (<= (length args) 2)
      (cons func (compile-list args env))
      (list func
            (compile (car args) env)
            (compile-binop func (cdr args) env) )))

(defun extend-env(formals env)
  (cond
    ((null formals)
     env )

    ((symbolp formals)
     (cons (cons formals (gensym "loc")) env) )

    ((consp formals)
     (extend-env (cdr formals) (extend-env (car formals) env)) )

    (t
     (error 1 formals "Invalid formal argument") )))

(defun targets(bindings)
  (if (null bindings)
      nil
      (cons (caar bindings) (targets (cdr bindings))) ))

(defun values(bindings)
  (if (null bindings)
      nil
      (cons (cadr (car bindings)) (values (cdr bindings))) ))

(defun progn-list(l)
  (if (> (length l) 1)
      (cons 'progn l)
      (car l) ))

(defun compile-list(l env)
  (cond
    ((null l)
     nil)

    ((atom l)
     (compile l env))

    ((null (cdr l))
     (list (compile (car l) env)) )

    (t
     (cons (compile (car l) env)
           (compile-list (cdr l) env) ))))

(defun lambdap(l)
  (cond
   ((not (consp l))
    nil )
   ((not (eq (pop l) lambda))
    nil )
   ((not (consp l))
    nil )
   ((symbolp (car l))
    t )
   ((atom l)
    (error 1 l "Expecting lambda args/body") )
   (t
     (if (apply and (mapc symbolp (car l)))
         t
         (error 1 l "Expecting lambda args/body") ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq expr
      '(let ((a 1)
             (b '(1 2 3))
             (f '(lambda (x) (+ x 1)))
             (g '(lambda (x) (- x 1))))
            (cond
              ((eq a 2) (+ (f a) (car (cdr b)) (car b)))
              ((eq a 1) (g (car b)))
              (t (g (f (car b)))) )))

(pprint expr)

(println)

(setq out (compile expr nil))

(pprint out)

(println (eval out))

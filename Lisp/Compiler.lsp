(defkey :vm-store)
(defkey :vm-push)
(defkey :vm-drop)
(defkey :vm-fetch)
(defkey :vm-add)
(defkey :vm-neg)
(defkey :vm-sub)
(defkey :vm-mul)
(defkey :vm-div)
(defkey :vm-and)
(defkey :vm-or)
(defkey :vm-bra)
(defkey :vm-brt)
(defkey :vm-brn)
(defkey :vm-call)
(defkey :vm-ret)

(setq types nil)
(setq loops nil)
(setq loope nil)
(setq env nil)
(setq code nil)
(setq fragments nil)
(setq fragstack nil)

(defun compiler(expr)
  (emit 'start)
  (compile expr)
  (push (reverse code) fragments))

(defun compile(expr)
  (cond
    ((null expr) (emit :vm-push nil))
    ((symbolp expr) (emit :vm-fetch (ref expr)))
    ((atom expr) (emit :vm-push expr))
    (t
     (if (lambdap (car expr))
           (progn
             (for-each (arg (cdr expr))
                       (compile arg) )
             (let ((sub (compile-lambda (cdar expr))))
                  (emit :vm-call sub) ))
           (case (car expr)
             (car    (emit (compile (cdr expr)) :vm-car))
             (cdr    (emit (compile (cdr expr)) :vm-cdr))
             (+      (compile-binary :vm-add :undefined (cdr expr) 0))
             (-      (compile-binary :vm-sub :vm-neg    (cdr expr) 0))
             (*      (compile-binary :vm-mul :undefined (cdr expr) 1))
             (and    (compile-binary :vm-and :undefined (cdr expr) t))
             (or     (compile-binary :vm-or  :undefined (cdr expr) nil))
             (/      (compile-/ (cdr expr)))
             (cons   (compile-cons (cdr expr)))
             (if     (compile-if (cdr expr)))
             (loop   (compile-loop (cdr expr)))
             (break  (compile-break (cdr expr)))
             (progn  (compile-progn (cdr expr)))
             (lambda (compile-lambda (cdr expr))) ))))
  nil )

(defun ref(sym)
  (let ((r (assoc sym env)))
       (if r (cdr r) sym) ))

(defun lambdap(x)
  (and (consp x) (eq (car x) lambda)) )

(defun compile-binary(op op1 args def)
  (cond
    ((null args)
     (if (not (eq :undefined def))
         (emit :vm-push def) ))
    ((null (cdr args))
     (compile (car args))
     (if (not (eq :undefined op1))
         (emit op1) ))
    (t  (compile (pop args))
        (compile-binary-aux op args) )))

(defun compile-binary-aux(op args)
  (when args
        (compile (pop args))
        (emit op)
        (compile-binary-aux op args) ))

(defun compile-/(args)
  (if (> (length args) 2) (error 1 args "Too many args to /"))
  (if (null (cdr args))
      (push 1 args))
  (compile (car args))
  (compile (cadr args))
  (emit :vm-div) )

(defun compile-cons(args)
  (if (<> (length args) 2) (error 1 args "Bad args to cons"))
  (compile (pop args))
  (compile (car args))
  (emit :vm-call '$mkcons) )

(defun compile-if(args)
  (let
    ((lelse) (lend))
    (compile (pop args))
    (setq lelse (gensym "lbl"))
    (emit :vm-brn lelse)
    (compile (pop args))
    (if (not (null args))
        (progn
          (setq lend (gensym "lbl"))
          (emit :vm-bra lend)
          (emit lelse)
          (compile (car args))
          (emit lend))
        (emit lelse) )))

(defun compile-loop(args)
  (push (gensym "lbl") loops)
  (push (gensym "lbl") loope)
  (emit (car loops))
  (compile-progn args)
  (emit :vm-drop)
  (emit :vm-bra (pop loops))
  (emit (pop loope)) )

(defun compile-break(args)
  (if (null loope) (error 1 :none "break outside loop"))
  (compile (car args))
  (emit :vm-bra (car loope)) )

(defun compile-progn(args)
  (emit :vm-push nil)
  (for-each (arg args) (emit :vm-drop) (compile arg)) )

(defun compile-lambda(args)
  (push code fragstack)
  (setq code nil)
  (let ((oldenv env)(tmp)(entry (gensym "lambda")))
    (emit entry)
    (for-each (x (reverse (car args)))
              (setq tmp (gensym "tmp"))
              (push (cons x tmp) env)
              (emit :vm-store tmp) )
    (compile-progn (cdr args))

    (setq env oldenv)
    (emit :vm-ret)
    (push (reverse code) fragments)
    (setq code (pop fragstack))
    entry ))

(defun emit c
  (push (if (keyp (car c)) c (car c)) code) )

(defun list()
  (for-each(f fragments)
    (println "------------------------------------")
    (for-each (i f)
      (if (atom i)
          (println i)
          (println "          " i) ))))


(defun optimise()
  (delete-push-pop))

(defun delete-push-pop()
  (let ((f fragments))
    (setq fragments nil)
    (for-each (block f)
      (push (dpp-block block) fragments) ))
      (setq fragments (reverse fragments)) )

(defun dpp-block(b)
  (if
    (< (length b) 2)
    b
    (if
      (and
        (consp (car b))
        (consp (cadr b))
        (eq (car (cadr b)) :vm-drop)
        (or
          (eq (caar b) :vm-push)
          (eq (caar b) :vm-fetch) ))
      (dpp-block (cddr b))
      (cons (car b) (dpp-block (cdr b))) )))

(defun next-op (here)
  (loop
    (until (null here) nil)
    (println here)
    (until (keyp (caar here)) here)
    (pop here) ))

(setq demo
  '((lambda (a b c)
      (loop
        ((lambda (a) (cons 4 a)) 3)
        (if b (break k))
        (+ 2 a))) 10 11 12))

(pprint demo)
(compiler demo)

(optimise)
(list)

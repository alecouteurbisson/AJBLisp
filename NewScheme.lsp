;; An environment is an association list paired with a list that is
;; the ambient environment

(defkey :lambda)
(defkey :closure)

;; Create a new current environment
(defun newenv ()
  (push nil $env) )

(defun popenv ()
  (when (eq $env $global)
           (error 1 :none "Can't pop global environment") )
  (pop $env) )

;; Find a symbol in the environment returning the key, value pair
;; if found or nil if not
(defun findenv(s (e . $env))
  (if (not e)
      nil
      (let ((r (assoc s (car e))))
         (if r r
	       (findenv s (cdr e)) ))))

;;
(defun getenv (s (e . $env))
  (let ((p (findenv s e)))
        (if p (cdr p) :undefined) ))

(defun setenv (s v (e . $env))
  (let ((p (findenv s e)))
       (if p
          (rplacd p v)
	        (rplaca e (cons (cons s v) (car e))))
       v ))

(defun local (s init (e . $env))
  (rplaca e (cons (cons s init) (car e))))


;; The global environment
(setq $global '(nil))
;; The current environment
(setq $env $global)

;; Some scheme definitions
(defun $set! (s v (e . $env))
  (setenv (s v e)) )

(defun $eval (x (e . '(nil)))
  (debug 1 $eval (x))
  (cond
    ((null x) nil)

    ((symbolp x)
     (getenv x e))

    ((not (consp x)) x)

    (($lambdap x)
      ($eval-lambda x))

    (t
     ($eval-expr x e) )))

(defun $eval-lambda (f (e . '(nil)))
  (debug 1 $eval-lambda (f))
  (list :closure (cadr f) e (cddr f)) )

(defun $eval-expr (x (e . '(nil)))
  (debug 1 $eval-expr (x))
  (let ((fun (car x))
         (args (cdr x))
         (result :undefined) )
        (for (i 1 3)
               (debug 1 $eval-expr (fun args))
               (when (execp fun)
                        (setq result ($apply-prim fun args e))
                        (break) )
               (when ($closurep fun)
                        (setq result ($eval-closure fun args e))
                        (break result) )
               (when (= i 3)
                        (error 1 fun "$eval-expr: Can't find a function") )
               (setq fun ($eval fun $global))
               (debug 1 $eval-expr (fun)) )
        (debug 1 eval-expr (result))
        result ))

(defun $apply (fun args (e . '(nil)))
  (cond
    (($closurep fun)
     (break ($eval-closure fun args e)) )

    (setq fun ($eval fun e))

    (error 1 fun "$apply: can't find a function") ))

(defun $apply-prim (fun args (e . '(nil)))
  (debug 1 $apply-prim (fun args e))
  (apply fun ($evlist args e)) )

(defun $evlist (l e)
  (cond
    ((null l) nil)
    ((listp l)
     (cons ($eval (car l) e) ($evlist (cdr l) e)) )
    (t
      (error 1 l "$evlist: Not a list") )))

(defun $eval-closure (c args e)
  (slet ((fun (closure.func c))
         (formals (closure.formals c))
         (result) )
         (debug 1 $eval-closure (fun formals))
         (when (not (= (length formals) (length args)))
                  (error 1 a "$eval-closure: Invalid number of arguments") )
         (setq $env (cons (closure.env c) e))
         (debug 1 $eval-closure ($env))
         (debug 1 $eval-closure (formals args))
         (guard
           (loop (while args
                            (loop (while fun result)
                                    (setq result ($eval (pop fun) $env)) ))
                   (local (pop formals) ($eval (pop args) e) $env)
                   (debug 1 $eval-closure (formals args $env)) )
           (popenv) )
         result ))

(defun $closurep (c)
  (and (consp c) (eq (car c) :closure)) )

(defun $lambdap (c)
  (and (consp c) (eq (car c) :lambda)) )

(defun closure.func (c)
  (cadr (cddr c)) )

(defun closure.formals (c)
  (cadr c) )

(defun closure.env (c)
  (car (cddr c)) )

(defun scheme (fun)
  (local fun (eval fun) $global) )

;; Tests
(defmacro test (expr expect)
  (print expr)
  (let ((result (eval expr)))
       (if (equal result expect)
           (println " = " result)
           (println " != " expect " = " result) )))

(defun setup-test ()
  (setenv 'A 1)
  (setenv 'B 2)
  (setenv 'C 3)

  (setq e1 (newenv))
  (local 'X 10)
  (local 'A 11)
  (local 'B 12)

  (setq e2 (newenv))
  (local 'X 20)
  (local 'A 21)
  (local 'C 23) )

(setup-test)

(test (getenv 'A $global) 1)
(test (getenv 'X $global) :undefined)
(test (getenv 'A e1) 11)
(test (getenv 'C e1) 3)
(test (getenv 'X) 20)
(test (getenv 'C) 22)
(test (getenv 'A) 21)

(popenv)
(test (getenv 'A) 11)

(popenv)
(test (getenv 'A) 1)

(setup-test)

(defun make-primitives ()
  (scheme '+)
  (scheme '-)
  (scheme 'atom)
  (scheme 'cons)
  (scheme 'eq)
  (scheme 'list) )

(make-primitives)

;; Debugging stuff
(setq :debug 1)

(defmacro debug (debug~lvl debug~func debug~args)
  (when (>= debug~lvl :debug)
    (print debug~func ": ")
    (for-each (debug~arg debug~args)
      (print debug~arg " = " (eval debug~arg) " "))
    (println) ))

(defmacro debugr (debug~lvl debug~func debug~arg)
  (when (>= debug~lvl :debug)
    (print debug~func ": ")
    (print debug~arg " = " (eval debug~arg) " ")
    (println))
    (eval debug~arg))

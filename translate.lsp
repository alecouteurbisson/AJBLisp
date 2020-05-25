;; Translate SECD lisp code into AJBLisp code
;;
;; Transformations performed
;;
;; add                                 ==> +
;; 'f                                  ==> nil
;; 't                                  ==> t
;; '<n>                                ==> <n>
;; (let <expr> (<var> . <val>)... )    ==> (let ((<var> <val) ...) <expr>)
;; (letrec <expr> (<var> . <val>)... ) ==> (let ((<var> <val) ...) <expr>)
;; (lambda (<arg>) <body>)             ==> '(lambda (<args>) <body>)
;;
;;
;; All transformations are performed recursively

(backtrace 5)

(defun translate (expr)

  (cond
    ((null expr) nil)
    ((eq 'add expr) +)
    ((equal ''f expr) nil)
    ((equal ''nil expr) nil)
    ((atom expr) expr)
    ((and (eq (car expr) 'quote) (cdr expr) (numberp (cadr expr)))
     (cadr expr) )
    ((eq (car expr) 'quote) expr)
    ((and (eq (car expr) 'let) (cdr expr) (println "let " expr))
     (translet (cdr expr)))
    ((and (eq (car expr) 'letrec) (cdr expr) (println "letrec " expr))
     (translet (cdr expr)))
    ((eq (car expr) 'progn)
     (cons 'progn (translist expr)) )
    ((and (cdr expr) (cddr expr) (eq (car expr) lambda))
     (list 'quote (translamb (cadr expr) (cddr expr))) )
    (t
     (cons (translate (car expr)) (translate (cdr expr))) )))

(defun translet (expr)  (println "translet " expr)
  (let ((ex (translate (if (consp expr) (car expr) nil)))
        (bind (if (consp expr) (cdr expr) nil))
        (b))
       (for-each (x bind)
                 (push (list (car x)
                             (translist (cdr x)) ) b) )
       (list 'let b ex) ))

(defun translist (expr) (println "(translist " expr ")")
  (if (null expr)
      nil
      (cons (translate (car expr)) (translist (cdr expr)))) )

(defun translamb (args expr)
  (cons lambda (cons args (translate expr))) )


(setq compsrc
  '(letrec compile
    (compile lambda (e)
      (comp e (quote nil) (quote (4 21))))
    (comp lambda (e n c)
      (if (atom e)
          (cons (quote 1) (cons (location e n) c))
      (if (eq (car e) (quote quote))
          (cons (quote 2) (cons (car (cdr e)) c))
      (if (eq (car e) (quote add))
          (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons (quote 15) c)))
      (if (eq (car e) (quote sub))
          (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons (quote 16) c)))
      (if (eq (car e) (quote mul))
          (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons (quote 17) c)))
      (if (eq (car e) (quote div))
          (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons (quote 18) c)))
      (if (eq (car e) (quote rem))
          (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons (quote 19) c)))
      (if (eq (car e) (quote leq))
          (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons (quote 20) c)))
      (if (eq (car e) (quote eq))
          (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons (quote 14) c)))
      (if (eq (car e) (quote car))
          (comp (car (cdr e)) n (cons (quote 10) c))
      (if (eq (car e) (quote cdr))
          (comp (car (cdr e)) n (cons (quote 11) c))
      (if (eq (car e) (quote atom))
          (comp (car (cdr e)) n (cons (quote 12) c))
      (if (eq (car e) (quote cons))
          (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons (quote 13) c)))
      (if (eq (car e) (quote if))
          (let (comp (car (cdr e)) n (cons (quote 8)
                                      (cons thenpt (cons elsept c))))
            (thenpt comp (car (cdr (cdr e))) n (quote (9)))
            (elsept comp (car (cdr (cdr (cdr e)))) n (quote (9))) )
      (if (eq (car e) (quote lambda))
          (let (cons (quote 3) (cons body c))
            (body comp (car (cdr (cdr e))) (cons (car (cdr e)) n)
                                      (quote (5))) )
      (if (eq (car e) (quote let))
          (let (let (complis args n (cons (quote 3)
                             (cons body (cons (quote 4) c))))
                 (body comp (car (cdr e)) m (quote (5))))
             (m cons (vars (cdr (cdr e))) n)
             (args exprs (cdr (cdr e))))
      (if (eq (car e) (quote letrec))
          (let (let (cons (quote 6) (complis args m
                           (cons (quote 3) (cons body (cons (quote 7) c)))))
                 (body comp (car (cdr e)) m (quote (5))))
            (m cons (vars (cdr (cdr e))) n)
            (args exprs (cdr (cdr e))))
      (complis (cdr e) n (comp (car e) n (cons (quote 4) c)))))))))))))))))))))
    (complis lambda (e n c)
      (if (eq e (quote nil)) (cons (quote 2) (cons (quote nil) c))
          (complis (cdr e) n (comp (car e) n (cons (quote 13) c)))))
    (location lambda (e n)
      (letrec
        (if (member e (car n)) (cons (quote 0) (posn e (car n)))
            (incar (location e (cdr n))))
      (member lambda (e n)
            (if (eq n (quote nil)) (quote f)
            (if (eq e (car n)) (quote t) (member e (cdr n)))))
      (posn lambda (e n)
        (if (eq e (car n)) (quote 0) (add (quote 1) (posn e (cdr n)))))
      (incar lambda (l) (cons (add (quote 1) (car l)) (cdr l)))))
    (vars lambda (d)
      (if (eq d (quote nil)) (quote nil)
          (cons (car (car d)) (vars (cdr d)))))
    (exprs lambda (d)
      (if (eq d (quote nil)) (quote nil)
          (cons (cdr (car d)) (exprs (cdr d)))))) )



(setq tr (translate compsrc))

(pprint tr)


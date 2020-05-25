(defun $+ (x y c) (c (+ x y)))
(defun $- (x y c) (c (- x y)))
(defun $* (x y c) (c (* x y)))
(defun $/ (x y c) (c (/ x y)))
(defun $= (x y c) (c (= x y)))
(defun $eq (x y c) (c (eq x y)))
(defun $car (x c) (c (car x)))
(defun $cdr (x c) (c (cdr x)))
(defun $cons (x y c) (c (cons x y)))
(defun id (x) x)

(cons (+ 1 2) (+ 3 4))

($+ 1 2 '(lambda (t1) ($+ 3 4 '(lambda (t2) ($cons t1 t2 id)))))

;; Curry function for CPS transformation
;; This only curries when an argument is an expression
;; (if a b ..) requires special treatment to avoid
;; evaluating b prematurely
;;
(defun curry (expr)
  (if (atom expr)
      expr
      (curryargs (car expr) (cdr expr)) ))

(defun curryargs (f args (cargs))
  (cond
    ((null args)
     (cons f (reverse cargs)) )
    ((and (eq f 'if) cargs)
     (cons f (cons (car cargs) (mapc curry args))) )
    ((atom (car args))
     (curryargs f (cdr args) (cons (car args) cargs)) )
    (t
     (let ((tmp (gensym "t")))
          (list (list lambda
                      (list tmp)
                      (curryargs f (cdr args) (cons tmp cargs)) )
                (curry (car args)) )))))

(defun cps (expr (cont . 'cont))
;  (println "cps" " " expr)
  (cond
    ((atom expr)
     (list cont expr) )
    ;; because the condition has been curried we know it will be a symbol
    ;; and does not recurse - this is not just convenient it avoids
    ;; passing the condition to the continuation as well
    ((eq (car expr) 'if)
     (cons (car expr)
           (cons (cadr expr)
                 (mapc '(lambda (x) (cps x cont)) (cddr expr)) )))
    ;; lambda much like if but quoted
    ;; this time the arg list is passed unaltered
    ((eq (car expr) lambda)
     (cons 'quote
           (list (cons (car expr)
                 (cons (cadr expr)
                       (mapc '(lambda (x) (cps x cont)) (cddr expr)) )))))
    ;; if all args are atomic just pass them to the continuation
    ((apply and (mapc atom expr))
     (append expr (list cont)) )
;    ((and (atom (car expr)) (execp (eval (car expr))))
;     (append expr (list cont)) )
    ;; will args always be length 1 here?
    (t
      (cps (cadr expr) (cps (car expr) cont)) )))

(defun f(n)
  (if (= 1 n)
      1
      (* (f (- n 1)) n) ))

(pprint f)
(println)
(pprint (list 'defun 'f '(n cont) (apply cps (list (curry (car (cddr f)))))))

;;  (defun f (n cont)
;;    (println n)
;;    (sleep 1000)
;;    ($= 1
;;        n
;;        '(lambda (t0)
;;           (if t0
;;             (cont 1)
;;             ($-
;;               n
;;               1
;;               '(lambda (t4) (f t4 '(lambda (t3) ($* t3 n cont)))) )))))
;;

(defun f
  (n cont)
  (= 1
     n
     '(lambda (t0)
              (if t0
                  (cont 1)
                  (- n
                     1
                     '(lambda (t4)
                              (f t4 (quote (lambda (t3) (* t3 n cont)))) ) )))))


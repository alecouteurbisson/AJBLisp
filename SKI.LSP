(backtrace 3)
;;; SKI compiler.
;;;
;;; Takes expressions of the form :-
;;;
;;; expr ::= (const int)
;;;        | (var var)
;;;        | (App expr expr)
;;;        | (lam var expr)
;;;
;;; and converts them into SKI combinators of the form
;;;
;;; SKI  ::= (S)
;;;        | (K)
;;;        | (I)
;;;        | ($ builtin)
;;;
;;; Also includes an evaluator which takes SKI combinators an reduces them
;;;
;;; See any good book on lambda calculus for what the above means.
;;; For example, I prefer :-
;;;   The Implementation of Functional Programming Languages
;;;   Simon L. Peyton Jones
;;;   Prentice Hall
;;;   ISBN 0-13-453325-9
;;; because, (as the title suggests) its about how functional languages can
;;; implemented as well as the theory behind them.
;;;
;;; The evaluators are a loose translation of ML/Miranda ones I wrote
;;; while I was taking a course on functional programming languages.
;;;
;;; Stephen J. Bevan <bevan@cs.man.ac.uk>
;;;-

(defun sk-mk-var (var) (list 'var var))
(defun sk-var? (expr) (eq (car expr) 'var))
(defun sk-var (var) (cadr var))

(defun sk-mk-const (int) (list 'const int))
(defun sk-const? (expr) (eq (car expr) 'const))
(defun sk-const (const) (cadr const))

(defun sk-mk-app (expr1 expr2) (list 'app expr1 expr2))
(defun sk-app? (expr) (eq (car expr) 'app))
(defun sk-app-lhs (expr) (cadr expr))
(defun sk-app-rhs (expr) (car (cddr expr)))

(defun sk-mk-lambda (var expr) (list 'lam var expr))
(defun sk-lambda? (expr) (eq (car expr) 'lam))
(defun sk-lambda-var (lam) (cadr lam))
(defun sk-lambda-expr (lam) (car (cddr lam)))

(defun sk-mk-s () (list 'S))
(defun sk-s? (expr) (eq (car expr) 'S))

(defun sk-mk-k () (list 'K))
(defun sk-k? (expr) (eq (car expr) 'K))

(defun sk-mk-i () (list 'I))
(defun sk-i? (expr) (eq (car expr) 'I))

(defun sk-mk-builtin (name) (list '$ name))
(defun sk-builtin? (expr) (eq (car expr) '$))
(defun sk-builtin (builtin) (cadr builtin))

;;;+fs
;;; Abstracts the variable `x' out of the expression `f'
;;;-
(defun sk-abstract(x f)
    (cond
     ((sk-app? f)
      (sk-mk-app
       (sk-mk-app (sk-mk-s) (sk-abstract x (sk-app-lhs f)))
       (sk-abstract x (sk-app-rhs f))))
     ((sk-var? f)
      (if (eq x (sk-var f))
	  (sk-mk-i)
	  (sk-mk-app (sk-mk-k) f)))
     (else (sk-mk-app (sk-mk-k) f))))

;;;+fs
;;; Compile the given lambda expression into SKI combinators.
;;;-
(defun sk-compile(expr)
    (cond
     ((sk-app? expr)
      (sk-mk-app (sk-compile (sk-app-lhs expr))
		 (sk-compile (sk-app-rhs expr))))
     ((sk-lambda? expr)
      (sk-abstract (sk-lambda-var expr)
		   (sk-compile (sk-lambda-expr expr))))
     (else expr)))

;;;+fs
;;; Pretty print the SKI expression
;;;-
(defun sk-pp (expr (lhs) (rhs))
  (cond
    ((sk-app? expr)
     (setq lhs (sk-app-lhs expr))
     (setq rhs (sk-app-rhs expr))
     (cond
       ((and (sk-app? lhs) (sk-s? (sk-app-lhs lhs)))
        (list 'S (sk-pp (sk-app-rhs lhs)) (sk-pp rhs)))
       ((sk-k? lhs) (list 'K (sk-pp rhs)))
       (t (list (sk-pp lhs) (sk-pp rhs))) ))
    ((sk-s? expr) 'S)
    ((sk-k? expr) 'K)
    ((sk-i? expr) 'I)
    ((sk-const? expr) (sk-const expr))
    ((sk-var? expr) (sk-var expr))
    ((sk-builtin? expr) (sk-builtin expr)) ))

;;;+fs
;;; Evaluate the SKI expression using SKI reduction rules
;;; I have my doubts about this function!
;;; I think I've implemented it wrong as it does not seem lazy enough in
;;; its evaluation.
;;;-
(defun sk-eval (expr (lhs) (rhs) (lhs2) (rhs2) (lhs3) (rhs3))
  (setq lhs (sk-app-lhs expr))
  (setq rhs (sk-app-rhs expr))
  (cond
    ;; handle I
    ((sk-i? lhs) rhs)
    ;; K only has one argument so return the expr unevaluated.
    ((sk-k? lhs) expr)
    ;; S only has one arg so again return it unevaluated.
    ((sk-s? lhs) expr)
    ;; All builtins are binary so not enough args yet.
    ((sk-builtin? lhs) expr)
    ;; Try an app of an app.
    ((sk-app? lhs)
     (setq lhs2 (sk-app-lhs lhs))
     (setq rhs2 (sk-app-rhs lhs))
     (cond
       ;; K has correct args so deal with it.
       ((sk-k? lhs2) rhs2)
       ;; S still does not have enough args.
       ((sk-s? lhs2) expr)
       ;; Try for an S with correct args.
       ((sk-app? lhs2)
        (setq lhs3 (sk-app-lhs lhs2))
        (setq rhs3 (sk-app-rhs lhs2))

	  (sk-eval (sk-mk-app (sk-eval (sk-mk-app rhs3 rhs))
				    (sk-eval (sk-mk-app rhs2 rhs)) )))
     (t (sk-eval (sk-mk-app (sk-eval lhs) rhs))) ))))

;;;+file-examples
;;; The rest is some examples of how to use the above.

;;; Some primitive numbers.
(setq zero '(lam 'x (lam 'y (var 'y))))
(setq one '(lam 'x (lam 'y (app (var 'x) (var 'y)))))
(setq two '(lam 'x (lam 'y (app (var 'x) (app (var 'x) (var 'y))))))

;;; The successor function for numbers.
(setq succ
  '(lam 'x
     (lam 'y
       (lam 'z (app (var 'y)
                    (app (app (var 'x) (var 'y))
                         (var 'z)))))))

;;; defun the sk equivalent of the above lambda expressions.
(setq sk-zero (sk-compile zero))
(setq sk-one (sk-compile one))
(setq sk-two (sk-compile two))
(setq sk-succ (sk-compile succ))

;;; Find the successor of zero
(sk-eval '(app sk-succ sk-zero))

;;;-file-examples

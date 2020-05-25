
;   (deconstruct  '(:x b :y :z) '(a b (c) d))
;-> ((:x . a) (:y c) (:z . d))

(defkey :fail)

(defun decons(pattern form)
  (catch :fail
    (let ((dc '(lambda (p f) (println p #\  f)
                (cond
                  ((null p)
                   (if (null f)
                       nil
                       (throw :fail :fail)) )

                  ((keyp p)
                   (list (cons p f)) )

                  ((atom p)
                   (if (eq p f)
                       nil
                       (throw :fail :fail)) )

                  (t (append (dc (car p) (car f))
                             (dc (cdr p) (cdr f)) ))))))
         (dc pattern form) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defkey :args :body :bind)

(setq expr
      '(let ((a 1) (b 2))
            (c a b)
            (d a b)))


(defun targets(bindings)
  (if (null bindings)
      nil
      (cons (caar bindings) (targets (cdr bindings))) ))

(defun values(bindings)
  (if (null bindings)
      nil
      (cons (cadr (car bindings)) (values (cdr bindings))) ))

(defun compile-letx(expr)
  (let ((parts (deconstruct '(let :bind :body) expr)))
       (when (eq :fail parts)
             (error 1 :none "Invalid let expr"))
       (append (list 'lambda
                     (targets (cdrassoc :bind parts))
                     (cdrassoc :body parts) )
               (values (cdrassoc :bind parts)) )))


(defun progn-list(l)
  (if (> (length l) 1)
      (cons 'progn l)
      (car l) ))

(defun compile-let(expr)
  (pop expr)
  (let ((bindings (car expr))
        (body (progn-list (cdr expr))) )
       (append (list (list 'lambda
                     (targets bindings)
                     body ))
             (values bindings) )))

(pprint (compile-let (car (cddr compile-letx))))

(pprint (compile-let expr))

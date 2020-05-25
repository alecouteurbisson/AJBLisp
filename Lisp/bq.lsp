(defmacro backquote (x)
  (bq x) )

(defun bq (x)
  (cond
    ((atom x) x)
    ((eq (car x) 'comma) (eval (cadr x)))
    ((and (consp (car x))
          (eq (caar x) 'comma-at) )
     (let ((splice (eval (car (cdar x)))))
       (if (consp splice)
           (append splice (bq (cdr x)))
           (error 1 splice "can't splice") )))
    (t
      (cons (bq (car x))
            (bq (cdr x)) ))))


(defmacro backquote2 (x)
  (bq2 x) )

(defun bq2 (x)
  (cond
    ((null x) nil)
    ((atom x) (list 'quote x))
    (t
      (let ((bqx (bqlist x)))
        (if (atom bqx)
            bqx
            (cons 'list bqx) )))))

(defun bqlist (x)
  (cond
    ((null x) nil)
    ((atom x) (list 'quote x))
    ((eq (car x) 'comma) (cadr x))
    ((and (consp (car x))
          (eq (caar x) 'comma-at) )
     (cons (list 'append (car (cdar x))
                         (bq2 (cdr x)) ) nil) )
    (t
     (cons (bq2 (car x)) (bqlist (cdr x))) )))

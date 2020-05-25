(defun cargs (x)
  (cons (car x)
        (cons (list (compile-args (cadr x) (cddr x)))
              (cddr x) )))

; Perform alpha substitution
(defun compile-args(args body)
  (cond
    ((null args)
     nil )
    ((symbolp args)
     (let ((f (gensym)))
       (nsubst f args body)
       f ))
    ((listp args)
     (cons
       (cond
         ((symbolp (car args))
          (let ((f (gensym)))
               (nsubst f (car args) body)
               f ))
         ((symbolp (caar args))
          (let ((f (gensym)))
            (nsubst f (caar args) body)
            (cons f (cdar args)) ))
         (t
           (error 1 args "Bad macro arguments") ))
       (compile-args (cdr args) body) ))
    (t
      (error 1 args "Bad macro arguments") )))


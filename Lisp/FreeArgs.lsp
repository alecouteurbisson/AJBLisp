(setq defmacro
  '(macro defn
     (set (car defn)
          (cons
            'macro
            (cons
              (free-args (cadr defn) (cddr defn))
              (cddr defn) )))
     (car defn) ))

; Destructively substitute a for b in c
(defun nsubst (a b c)
  (cond
    ((eq b c) a)
    ((atom c) c)
    (t
      (if (eq (car c) b)
        (rplaca c a)
        (nsubst a b (car c)) )
      (if (eq (cdr c) b)
        (rplacd c a)
        (nsubst a b (cdr c)) )))
  c )

; Make the arguments in args free symbols
(defun free-args(args body)
  (cond
    ((null args)
     nil )
    ((symbolp args)
     (let ((f (free args)))
       (nsubst f args body)
       f ))
    ((listp args)
     (cons
       (cond
         ((symbolp (car args))
          (let ((f (free (car args))))
               (nsubst f (car args) body)
               f ))
         ((symbolp (caar args))
          (let ((f (free (caar args))))
            (nsubst f (caar args) body)
            (cons f (cdar args)) ))
         (t
           (error 1 args "Bad macro arguments") ))
       (free-args (cdr args) body) ))
    (t
      (error 1 args "Bad macro arguments") )))

(defmacro defmacro defn
 (set (car defn)
      (cons
        'macro
        (cons
          (free-args (cadr defn) (cddr defn))
          (cddr defn) )))
 (car defn) )

(defmacro defun defn
 (set (car defn)
      (cons
        'lambda
        (cons
          (free-args (cadr defn) (cddr defn))
          (cddr defn) )))
 (car defn) )

(defun deep-copy (l)
  (if (consp l)
      (cons (deep-copy (car l)) (deep-copy (cdr l)))
      l ))

(let ((~free-args (deep-copy free-args)))
      (setq free-args ~free-args) )


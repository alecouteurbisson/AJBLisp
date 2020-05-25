(setq :editdir "F:\\Work\\Prog\\CPP\\AJBLisp\\V3.5\\")
(setq :editor (concat :editdir "SciTE.exe"))

(defun source(x y)
  (cond
    ((atom y) (list 'setq x y))
    ((vectorp y) (list 'setq x y))
    (t
      (case (car y)
        ('lambda (list 'defun x (cdr y)))
        ('macro (list 'defmacro x (cdr y)))
        (:else (list 'setq x (list 'quote y))) ))))


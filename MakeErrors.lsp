(defun make-errors ()
  (setq errno 0)
  (guard
    (setq in  (open-file "errors.in"   :read))
    (setq hdr (open-file "lisperr.h"   :create))
    (setq src (open-file "lisperr.cpp" :create))
    ; Write file headers
    (process-errors)
    ; Write file footers
    (progn
      (close-file in)
      (close-file hdr)
      (close-file src)) ))

(defun process-errors ()
  (loop
    (setq input (read f))
    (cond
      ((eq :eof input) (break))
      ((stringp input) (process-comment input))
      ((consp input)   (process-error input))
      (t (error 1 input "Invalid entry in errors.in")) )))

(defun process-comment (c)
  (println "// " c) )
  
(defun process-error (e)
  (cond
    ((numberp (car e))  (setq errno (car e)))
    ((eq (car e) '+)    (inc errno))
    ((eq (car e) '-)    (dec errno))
    (t (error 1 none: "Invalid entry in errors.in")) )
  (setq errsym (cadr e))
  (setq errmsg (car (cddr e)))
  (unless errmsg (setq errmsg ""))
  (write-hdr errno errsym errmsg)
  ; And the rest...
)

(defun write-hdr (n s m)
  (println :space :space s #\= n))

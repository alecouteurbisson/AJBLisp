(backtrace 3)

;; The location of the system lisp source files
(defconstant :lispdir "c:/Program Files/AJBLisp/")

; Set the locale
;;(locale "English_United Kingdom.850")

; Set up the time zone
(tzset "GMT0BST")

; defun and defmacro are declared using setq for obvious reasons
(setq defun
  '(macro defun~defn
     (set (car defun~defn)
          (cons 'lambda (cdr defun~defn)) )
     (car defun~defn) ))

; defun and defmacro are declared using setq for obvious reasons
(setq defmacro
  '(macro defmacro~defn
     (set (car defmacro~defn)
          (cons 'macro (cdr defmacro~defn)) )
     (car defmacro~defn) ))

;; Load the pretty printer
(load (concat :lispdir "pprint.lsp"))

;; Load the sort function
(load (concat :lispdir "sort.lsp"))

;; Load the directory lister
(load (concat :lispdir "ls.lsp"))

; Substitute a for b in c
(defun subst (a b c)
  (cond
    ((eq b c) a)
    ((atom c) c)
    (t (cons (subst a b (car c))
             (subst a b (cdr c)) ))))

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
        (nsubst a b (cdr c)) )
      c )))


; Print newline
(defun terpri () (println))

; Read until valid input received
; Ignores and discards invalid input following valid input
; e.g entering: 1 2 3) will read 1
(defun saferead((prompt . ">> ") (msg . "Error in input - Please re-enter"))
  (let ((x))
    (loop
      (print prompt)
      (flush)
      (setq x (errorset (read)))
      (until (listp x) (flush) (car x))
      (println msg) )))

; Trace function execution
; Don't trace functions used in trace itself
(defmacro trace fn
  (setq fn (car fn))
  (put fn 'olddef (eval fn))
  (set fn (subst fn 'fn
                 '(lambda trace~x
                    (setq trace~x (mapc eval trace~x))
                    (println "trace:  " (cons 'fn trace~x))
                    (setq trace~x (apply (get 'fn 'olddef) trace~x))
                    (println "return: " 'fn " = " trace~x)
                    trace~x)))
  fn)

(defmacro untrace fn
  (setq fn (car fn))
  (set fn (get fn 'olddef))
  (remprop fn 'olddef)
  (list fn 'untraced))

;; Return a sorted list of objects
(defun obs ()
  (sort (mapc string (oblist)) string>) )

;; Display the local date, time and zone
(local-time t)
(println (time-string "%#c %Z\n"))

(collect)

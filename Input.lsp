;;; Line buffer
;;; Actually buffers tokens rather than characters
(setq line nil)
(setq prompt nil)
(setq type-ahead nil)

;;; Simulate refilling line buffer
(defun get-line ()
  (if prompt
      (progn
        (princ ">> ")
        (loop
          (until (eq type-ahead line))
          (setq nxt (car type-ahead))
          (princ :stderr (pop type-ahead))
          (if (not (eql "(" nxt))
               (princ :space) ))
        (setq prompt nil) ))
  (setq line (tokenise (readline))) )

;;; read-token is the analogue of inch
(defun read-token ()
  (loop
    (while (null line))
    (get-line) )
  (pop line) )

;;; unget-token is the analogue of ungetch
(defun unget-token (tok)
  (push tok line) )

;;; Emulate read for symbols, integers and lists
(defun READ nil
  (let ((tok (read-token)))
       (cond
         ((symbolp tok) tok)
         ((numberp tok) tok)
         ((eql tok "(") (READ-LIST))
         ((eql tok ")") (error 1 :none "Unexpected )"))
         (t (error 1 tok "Can't read this")) )))

(defun READ-LIST ()
  (let ((tok (read-token)))
       (cond
         ((eql tok ")")
           nil )
         (t
          (unget-token tok)
          (cons (READ) (READ-LIST)) ))))

(defun rpl ()
  (let ((input))
       (loop
         (setq prompt t type-ahead line)
         (setq input (READ))
         (setq prompt nil)
         (printc "== " input)
         (until (eq input 'x)) )))


; Binary ops may be left/right/non associative
; Prefix ops may be left/non associative
; Postfix ops may be right/non associative
;
; a + b | *  -> Shift (higher precedence)
; a * b | +  -> Reduce, Shift (lower precedence)
; a + b | +  -> Reduce, shift (Left associative)
; a ^ b | ^  -> Shift (Right associative)
; a = b | =  -> Error (Non associative)
; - | -      -> Shift (Left associative)
; a ! | !    -> Reduce, shift (Right associative)
; & | &      -> Error (Non associative)
;
; Ground rules:
; Prefix operators do not bind right
; Postfix operators do not bind left
; Matchfix operators have binding and non binding forms
; The right binding of a matchfix operator is applied
; to the matching (closing) part.
; A zero binding must be adjacent to an operator.
; A non-zero binding must be adjacent to a symbol/value
; Reduce simply collects the bound items and packages
; them.
;
; Example:
; Op left right match
; ,  10   11
; +  20   21                Infix +
; -  20   21                Infix -
; *  30   31                Infix *
; /  30   31                Infix /
; ^  40   39                Infix ^
; !  50   0                 Postfix !
; +  0    60                Prefix +
; -  0    60                Prefix -
; (  0    0      )          Parenthesis
; (  99   0      )          Function call
; [  0    0      ]          Brackets
; [  99   0      ]          Indexer
; (  0    99     )          C style Cast?
; /* 0    0      */         Comment operator?
;


(setq opstack nil)        ; The operator stack
(setq argstack nil)       ; The value stack

(setq operators nil)      ; The operator definitions

(defun define-op (op sym left right (match))
  (push (list op sym left right match) operators) )

(define-op "," seq 10 11)
(define-op "+" add 20 21)
(define-op "-" sub 20 21)
(define-op "*" mul 20 21)
(define-op "/" div 20 21)
(define-op "^" div 40 39)
(define-op "+" pos  0 60)
(define-op "!" pos 50  0)
(define-op "-" neg  0 60)
(define-op "(" par  0  0 ")")
(define-op "(" fun 99  0 ")")
(define-op "[" idx 99  0 "]")

(defun parse ()
  (setq opstack   nil        ; The operator stack
        argstack  nil        ; The value stack
        operator  nil        ; The operator definition
	      bindl     -1         ; The stacks binding to the value at TOS
	      tos-arg   nil)       ; True if the argstack was pushed last
  (loop
    (while (tokens) t)
    (let
      ((tok   (pop tokens))
       (bindr (or (null tokens)            ; True if operator or EOI next
		              (operatorp (car tokens))))
       (operator) )

      (case (typeof tok)
        ((:symbol :integer :float)         ; A value
	       (when (tos-arg) (error 1 tok "Consecutive symbols"))
	       (push tok argstack)
	       (setq tos-arg nil) )

        (:string                           ; An operator
	       (setq operator
	             (find-op tok (if tos-arg 0 bindl) bindr))  ; Get the matching op



(defun find-op (tok bl br)
  ; convert bl to a bool, br is already boolean
  (setq bl (

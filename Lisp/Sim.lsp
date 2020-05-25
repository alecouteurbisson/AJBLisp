(defconstant :0 0)  ;; Hard Low
(defconstant :1 1)  ;; Hard High
(defconstant :L 2)  ;; Pull Down
(defconstant :H 3)  ;; Pull Up
(defconstant :Z 4)  ;; High Impedance
(defconstant :- 5)  ;; Don't Know
(defconstant :X 6)  ;; Potential Conflict

;; Both :- and :X propogate as themselves but :X will override :-
;; Never use :X as an input (always use :-) and then the appearance
;; of :X properly represents a (potential) conflict in the circuit.
;; E.g, (mvlWire :- :1) -> :X  a potential conflict
;; and  (mvlWire :1 :0) -> :X  an actual conflict
;; but  (mvlWire :- :L) -> :-  no conflict with pull down
;;
;; In a properly designed circuit :X should never appear after any
;; initial reset; but :- might, especially at inputs.

(defconstant TTBuf
;;  :0 :1 :L :H :Z :- :X
  #(:0 :1 :0 :1 :X :- :X) )

(defconstant TTNot
;;  :0 :1 :L :H :Z :- :X
  #(:1 :0 :1 :0 :X :- :X) )

(defconstant TTResolve
;;    :0 :1 :L :H :Z :- :X
  #(#(:0 :X :0 :0 :0 :X :X)    ; :0
    #(:X :1 :1 :1 :1 :X :X)    ; :1
    #(:0 :1 :L :X :L :- :X)    ; :L
    #(:0 :1 :X :H :H :- :X)    ; :H
    #(:0 :1 :L :H :Z :- :X)    ; :Z
    #(:X :X :- :- :- :X :X)    ; :-
    #(:X :X :X :X :X :X :X)) ) ; :X

(defconstant TTAnd
;;    :0 :1 :L :H :Z :- :X
  #(#(:0 :0 :0 :0 :0 :0 :0)    ; :0
    #(:0 :1 :0 :1 :X :- :X)    ; :1
    #(:0 :0 :0 :0 :0 :0 :0)    ; :L
    #(:0 :1 :0 :1 :X :- :X)    ; :H
    #(:0 :X :0 :X :X :- :X)    ; :Z
    #(:0 :- :0 :- :- :- :X)    ; :-
    #(:0 :X :0 :X :X :X :X)) ) ; :X

(defconstant TTOr
;;    :0 :1 :L :H :Z :- :X
  #(#(:0 :1 :0 :1 :X :- :X)    ; :0
    #(:1 :1 :1 :1 :1 :1 :1)    ; :1
    #(:0 :1 :0 :1 :X :- :X)    ; :L
    #(:1 :1 :1 :1 :1 :1 :1)    ; :H
    #(:X :1 :X :1 :X :- :X)    ; :Z
    #(:- :1 :- :1 :- :- :X)    ; :-
    #(:X :1 :X :1 :X :X :X) )) ; :X

(defconstant TTXor
;;    :0 :1 :L :H :Z :- :X
  #(#(:0 :1 :0 :1 :X :- :X)    ; :0
    #(:1 :0 :1 :0 :X :- :X)    ; :1
    #(:0 :1 :0 :1 :X :- :X)    ; :L
    #(:1 :0 :1 :0 :X :- :X)    ; :H
    #(:X :X :X :X :X :- :X)    ; :Z
    #(:- :- :- :- :- :- :X)    ; :-
    #(:X :X :X :X :X :X :X) )) ; :X

(defun true(a)
  (or (eq a :1) (eq a :H)) )

(defun false(a)
  (or (eq a :0) (eq a :L)) )

(defun undef(a)
  (or (eq a :-) (eq a :X)) )

(defun mvlUnary(tt a)
  (vref tt (eval a)) )

(defun mvlBinary(tt a b)
  (vref (vref tt (eval a)) (eval b)) )

(defun mvlWire (a b)
  (mvlBinary TTResolve a b) )

(defun mvlBuffer (a)
  (mvlUnary TTBuf a) )

(defun mvlInvert (a)
  (mvlUnary TTNot a) )

(defun mvlAnd (a b)
  (mvlBinary TTAnd a b) )

(defun mvlOr (a b)
  (mvlBinary TTOr a b) )

(defun mvlNand (a b)
  (mvlInvert (mvlBinary TTAnd a b)) )

(defun mvlNor (a b)
  (mvlInvert (mvlBinary TTOr a b)) )

(defun mvlXor (a b)
  (mvlBinary TTXor a b) )

(defun mvlXnor (a b)
  (mvlInvert (mvlBinary TTXor a b)) )

(defun pulse nodes
  (for-each (n nodes) (rplaca n (cdr n)) (print (car n) :space))
  (println) )

(defmacro setl(n v) (rplacd (eval n) (eval v)))

(defun getl(n) (car n))

(setq ina  '(:1 . :1))
(setq inb  '(:1 . :1))
(setq outa '(:- . :-))
(setq outb '(:- . :-))

(defun net nil
    (setl outa (mvlNand (getl ina) (getl outb)))
    (setl outb (mvlNand (getl inb) (getl outa)))
    (setl outa (mvlNand (getl ina) (getl outb)))
    (setl outb (mvlNand (getl inb) (getl outa)))
    (setl outa (mvlNand (getl ina) (getl outb)))
    (setl outb (mvlNand (getl inb) (getl outa)))
    (setl outa (mvlNand (getl ina) (getl outb)))
    (setl outb (mvlNand (getl inb) (getl outa))) )

(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(setl ina ':0)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(setl ina ':1)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(setl inb ':0)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(setl inb ':1)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(setl ina ':0)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(setl ina ':1)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)
(net)
(pulse ina inb outa outb)

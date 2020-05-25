(defconstant :0 0)
(defconstant :1 1)
(defconstant :L 2)
(defconstant :L 3)
(defconstant :Z 4)
(defconstant :X 5)

defconstant TTBuf
;;  :0 :1 :L :H :Z :X
  #(:0 :1 :0 :1 :X :X)

defconstant TTNot
;;  :0 :1 :L :H :Z :X
  #(:1 :0 :1 :0 :X :X)

(defconstant TTResolve
;;    :0 :1 :L :H :Z :X
  #(#(:0 :X :0 :0 :0 :X)  ; :0
    #(:X :1 :1 :1 :1 :X)  ; :1
    #(:0 :1 :L :X :L :X)  ; :L
    #(:0 :1 :X :H :H :X)  ; :H
    #(:0 :1 :L :H :Z :X)  ; :Z
    #(:X :X :X :X :X :X)) ; :X

(defconstant TTAnd
;;    :0 :1 :L :H :Z :X
  #(#(:0 :0 :0 :0 :0 :0)  ; :0
    #(:0 :1 :0 :1 :X :X)  ; :1
    #(:0 :0 :0 :0 :0 :0)  ; :L
    #(:0 :1 :0 :1 :X :X)  ; :H
    #(:0 :X :0 :X :X :X)  ; :Z
    #(:0 :X :0 :X :X :X)) ; :X

(defconstant TTNand
;;    :0 :1 :L :H :Z :X
  #(#(:1 :1 :1 :1 :1 :1)  ; :0
    #(:1 :0 :1 :0 :X :X)  ; :1
    #(:1 :1 :1 :1 :1 :1)  ; :L
    #(:1 :0 :1 :0 :X :X)  ; :H
    #(:1 :X :1 :X :X :X)  ; :Z
    #(:1 :X :1 :X :X :X)) ; :X

(defconstant TTOr
;;    :0 :1 :L :H :Z :X
  #(#(:0 :1 :0 :1 :X :X)  ; :0
    #(:1 :1 :1 :1 :1 :1)  ; :1
    #(:0 :1 :0 :1 :X :X)  ; :L
    #(:1 :1 :1 :1 :1 :1)  ; :H
    #(:X :1 :X :1 :X :X)  ; :Z
    #(:X :1 :X :1 :X :X)) ; :X

(defconstant TTNor
;;    :0 :1 :L :H :Z :X
  #(#(:1 :0 :1 :0 :X :X)  ; :0
    #(:0 :0 :0 :0 :0 :0)  ; :1
    #(:1 :0 :1 :0 :X :X)  ; :L
    #(:0 :0 :0 :0 :0 :0)  ; :H
    #(:X :0 :X :0 :X :X)  ; :Z
    #(:X :0 :X :0 :X :X)) ; :X

(defconstant TTXor
;;    :0 :1 :L :H :Z :X
  #(#(:0 :1 :0 :1 :X :X)  ; :0
    #(:1 :0 :1 :0 :X :X)  ; :1
    #(:0 :1 :0 :1 :X :X)  ; :L
    #(:1 :0 :1 :0 :X :X)  ; :H
    #(:X :X :X :X :X :X)  ; :Z
    #(:X :X :X :X :X :X)) ; :X

(defconstant TTXnor
;;    :0 :1 :L :H :Z :X
  #(#(:1 :0 :1 :0 :X :X)  ; :0
    #(:0 :1 :0 :1 :X :X)  ; :1
    #(:1 :0 :1 :0 :X :X)  ; :L
    #(:0 :1 :0 :1 :X :X)  ; :H
    #(:X :X :X :X :X :X)  ; :Z
    #(:X :X :X :X :X :X)) ; :X

(defun mvlAnd (a b)
  (

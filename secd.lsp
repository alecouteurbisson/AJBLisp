  ; SECD  Base_0 compiler
 
(defconstant $ld 1)
(defconstant $ldc 2)
(defconstant $ldf 3)
(defconstant $rtn 5) 
(defconstant $dum 6)
(defconstant $dec 7)  
(defconstant $int 8) 
(defconstant $inc 9) 
(defconstant $car 10)
(defconstant $cdr 11) 
(defconstant $atom 12)
(defconstant $cons 13) 
(defconstant $eq 14)
(defconstant $stop 21)
(defconstant $add 22)
(defconstant $sub 23)
(defconstant $mul 24)
(defconstant $div 25) 
(defconstant $rem 26) 
(defconstant $leq 27)
(defconstant $lexleq 28) 
(defconstant $let 32)
(defconstant $trlet 33)
(defconstant $letrec 34) 
(defconstant $trletr 35) 
(defconstant $app 36)  
(defconstant $tr_app 37)  
(defconstant $sel 38)
(defconstant $tr_sel 39)
 
  (defun compile (e)
    (comp e nil (quote ($app $stop))))

  (defun comp (e n c)
    (cond
      ((integerp e)
       (cons $ldc (cons e c)) )
      ((atom e)
        (cons (add $ld (location e n)) c) )
      ((eq e nil)
        (cons $ldc (cons e c)) )
      (t (let* 
          (if (atom he)
              (if (lexleq he (quote inc))
                  (if (lexleq he (quote cons))
                      (comp_00 he te n c)
                      (comp_01 he te n c) ) 
                  (if (lexleq he (quote lexleq))
                      (comp_10 he te n c)
                      (comp_11 he te n c) ) ) 
              (comp_user he te n c))    
          (he car e)
          (te cdr e)) )))

  (defun comp_00 (he te n c)
    (if (eq he (quote cons))
        (comp (car (cdr te)) n (comp (car te) n (cons $cons c)))
    (if (eq he (quote and))
        (comp (cons (quote if) 
                    (cons (car te) 
                          (cons (car (cdr te)) (quote ((quote F))))))
              n c)
    (comp1 
      he te n c 
      (test_cod he (quote ( (car . $car) (cdr . $cdr) (atom . $atom) )))
      (test_cod he (quote ( (add . $add) )))
  ))))
  
  (defun comp_01 (he te n c)
    (if (eq he (quote if))
        (let* (comp 
               (car te) n
               (if (eq $rtn (car c))
                   (cons $trsel (cons thenpt elsept))
                   (cons $sel (cons thenpt (cons elsept c)))
               )) 
          (thenpt comp (car (cdr te))       n (quote ($rtn)) ) 
          (elsept comp (car (cdr (cdr te))) n (quote ($rtn)) ))
    (comp1 
      he te n c 
      (test_cod he (quote ( (1+ . $inc) (dec . $dec) )))
      (test_cod he (quote ( (eq . $eq) (div . $div) )))
  )))

  (defun comp_10 (he te n c)
    (if (eq he (quote lambda))
        (let* (cons $ldf (cons body c))
               (body comp 
                 (car (cdr te)) 
                 (cons (car te) n)
                 (quote ($rtn))))
    (if (eq he (quote let*   )) (comp_block he te n c $let   )             
    (if (eq he (quote letrec)) (comp_block he te n c $letrec)             
    (comp1 
      he te n c 
      (test_cod he (quote ( (integerp . $int) )))
      (test_cod he (quote ( (leq . $leq) (lexleq . $lexleq) )))
  )))))

  (defun comp_11 (he te n c)
    (if (eq he (quote quote))
        (cons $ldc (cons (car te) c))
    (if (eq he (quote or))
        (comp (cons (quote if) 
                    (cons (car te) 
                          (cons (quote (quote T)) (cons (car (cdr te)) nil)))) 
              n c)
    (comp1 
      he te n c 
      (test_cod he (quote (  )))
      (test_cod he (quote ( (sub . $sub) (mul . $mul) (rem . $rem) )))
  ))))

  (defun comp1 (he te n c i_1 i_2)
    (if (integerp i_1) (comp (car te) n (cons i_1 c))
    (if (integerp i_2) (comp (car te) n (comp (car (cdr te)) n (cons i_2 c)))
    (comp_user he te n c)
  )))

  (defun comp_block(he te n c i_let)
        (let* (let* (let* (let*
                           (if (eq he (quote let*)) 
                               (complis args n block)
                               (cons $dum (complis args m block)))
                       (block if rtnok
                          (cons blhead body)
                          (cons blhead (cons body c)))) 
                  (body comp (car te) m (quote ($rtn))))
             (blhead if rtnok     
                        (add $ld (count args i_let))
                        (count args i_let)))
         (rtnok eq $rtn (car c))               
         (m cons (mapcar (lambda(x)(car x)) (cdr te)) n)
         (args mapcar (lambda(x)(cdr x)) (cdr te))))

  (defun comp_user(he te n c)
    (complis te n 
         (comp he n 
            (if (eq $rtn (car c))
                (cons (count te $trapp) nil)
                (cons (count te $app) c)))))

  (defun test_cod (e n)
      (if (eq n nil) nil
          (if (eq e (car (car n))) 
              (cdr (car n))
              (test_cod e (cdr n)))))

   (defun posn (e n)
     (if (null n)
         -1
         (if (eq e (car n)) 0 (1+ (posn e (cdr n))))))

  (defun complis (e n c)
    (if (eq e nil) c 
        (complis (cdr e) n (comp (car e) n c))))

  (defun count(l i)
    (if (eq l nil) i
        (1+ (count (cdr l) i))))
              
  (defun mapcar(mapcar~f mapcar~l)
    (pprint mapcar~l)
    (if (eq mapcar~l nil) nil
        (cons (mapcar~f (car mapcar~l)) (mapcar mapcar~f (cdr mapcar~l)))))

  (defun location (e n)
      (if (member e (car n)) 
          (posn e (car n))
          (add 256 (location e (cdr n))))
  )
    
  (defmacro let* q
    (eval (cons let (append (rewrite-bindings (cadr q)) (list (car q))))) )
 
  (defun rewrite-bindings q
    (list
      (mapcar '(lambda x (cons (caar x) (cdar x)))
                  q )))
    
  (setq add +)
  
  (when (eq oldcar :undefined)
      (setq oldcar car
              car '(lambda (x) (if  (and x (not (eq :undefined x)))
                                         (oldcar x)
                                         nil ))
              oldcdr cdr
              cdr '(lambda (x) (if  (and x (not (eq :undefined x)))
                                         (oldcdr x)
                                         nil )) ))
  
  (defun lexleq(x y)
    (string<= (string x) (string y)) )

  (defun 1+ (i)
    (+ 1 i) )
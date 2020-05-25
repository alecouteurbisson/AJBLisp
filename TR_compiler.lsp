  1 LD
  2 LDC
  3 LDF
  4
  5 RTN
  6 DUM
  7
  8 INT
  9 INC
 10 CAR
 11 CDR
 12 ATOM
 13 CONS
 14 EQ
 15 APP
 16 TR_APP
 17 RAP
 18 TR_RAP
 19 SEL
 20 TR_SEL
 21 STOP


  (compile lambda (e)
    (comp e nil (quote ($APP $STOP))))

  (comp lambda (e n c)
    (if (atom e)
        (cons $LD (cons (location e n) c))
    (if (integer e)
        (cons $LDC (cons e c))
    (if (eq e nil)
        (cons $LDC (cons e c))

    (if (eq (car e) (quote quote))
        (cons $LDC (cons (car (cdr e)) c))

    (if (eq (car e) (quote inc))
        (comp (car (cdr e)) n (cons $INC c))

    (if (eq (car e) (quote integer))
        (comp (car (cdr e)) n (cons $INT c))

    (if (eq (car e) (quote eq))
        (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons $EQ c)))
    (if (eq (car e) (quote car))
        (comp (car (cdr e)) n (cons $CAR c))
    (if (eq (car e) (quote cdr))
        (comp (car (cdr e)) n (cons $CDR c))
    (if (eq (car e) (quote atom))
        (comp (car (cdr e)) n (cons $ATOM c))
    (if (eq (car e) (quote cons))
        (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons $CONS c)))
    (if (eq (car e) (quote if))
        (let (comp
               (car (cdr e)) n
               (if (eq (car c) $RTN)
                   (cons $TR_SEL (cons thenpt (cons elsept nil)))
                   (cons $SEL (cons thenpt (cons elsept c)))
               )
             )
          (thenpt comp (car (cdr (cdr e))) n (quote ($RTN)))
          (elsept comp (car (cdr (cdr (cdr e)))) n (quote ($RTN))) )
    (if (eq (car e) (quote lambda))
        (let (cons $LDF (cons body c))
          (body comp (car (cdr (cdr e))) (cons (car (cdr e)) n)
                                    (quote ($RTN))) )
    (if (eq (car e) (quote let))
        (let (let (complis args n (cons $LDF
                           (cons body (tail_comp $APP c))))
               (body comp (car (cdr e)) m (quote $RTN))))
           (m cons (vars (cdr (cdr e))) n)
           (args exprs (cdr (cdr e))))
    (if (eq (car e) (quote letrec))
        (let (let (cons $DUM (complis args m
                         (cons $LDF (cons body (tail_comp $RAPP c)))))
               (body comp (car (cdr e)) m (quote ($RTN))))
          (m cons (vars (cdr (cdr e))) n)
          (args exprs (cdr (cdr e))))
    (complis (cdr e) n (comp (car e) n (tail_comp $APP c)
  ))))))))))))))))))

  ;; Convert i from $RAPP or $APP to $TR_RAPP or $TR_APP if c is $RTN
  ;; then return (cons i c)
  ;; Conversion
  (tail_comp lambda (i c)
    (if (eq (car c) $RTN)
        (cons (inc i) nil)
        (cons i c) ))

  (complis lambda (e n c)
    (if (eq e nil) (cons $LDC (cons nil c))
        (complis (cdr e) n (comp (car e) n (cons $CONS c)))))

  (location lambda (e n)
    (letrec
      (if (member e (car n)) (cons 0 (posn e (car n)))
          (incar (location e (cdr n))))

  (posn lambda (e n)
      (if (eq e (car n)) 0 (inc (posn e (cdr n)))))

  (incar lambda (l) (cons (inc (car l)) (cdr l))) ))

  (vars lambda (d)
    (if (eq d nil) nil
        (cons (car (car d)) (vars (cdr d)))))

  (exprs lambda (d)
    (if (eq d nil) nil
        (cons (cdr (car d)) (exprs (cdr d)))))


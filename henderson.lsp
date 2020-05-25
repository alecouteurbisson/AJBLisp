(load "secd-vm.lsp")

(defun compile (e)
  (comp e 'nil '($stop)))

(defun comp (e n c)
  (cond

    ((null e)
     (cons '$ldc (cons nil c)) )

    ((symbolp e)
        (cons '$ld (cons (location e n) c)) )

    ((atom e)
        (cons '$ldc (cons e c)) )

    ((eq (car e) 'quote)
        (cons '$ldc (cons (car (cdr e)) c)) )

    ((eq (car e) 'add)
     (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons '$add c))) )

    ((eq (car e) 'sub)
     (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons '$sub c))) )

    ((eq (car e) 'mul)
     (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons '$mul c))) )

    ((eq (car e) 'div)
     (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons '$div c))) )

    ((eq (car e) 'rem)
     (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons '$rem c))) )

    ((eq (car e) 'leq)
     (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons '$leq c))) )

    ((eq (car e) 'eq)
     (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons '$eq c))) )

    ((eq (car e) 'car)
     (comp (car (cdr e)) n (cons '$car c)) )

    ((eq (car e) 'cdr)
     (comp (car (cdr e)) n (cons '$cdr c)) )

    ((eq (car e) 'atom)
     (comp (car (cdr e)) n (cons '$atom c)) )

    ((eq (car e) 'cons)
     (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons '$cons c))) )

    ((eq (car e) 'if)
     (let ((thenpt (comp (car (cdr (cdr e))) n '($join)))
           (elsept (comp (car (cdr (cdr (cdr e)))) n '($join))))
          (comp (car (cdr e)) n (cons '$sel (cons thenpt (cons elsept c)))) ))

    ((eq (car e) 'lambda)
     (let ((body (comp (car (cdr (cdr e))) (cons (car (cdr e)) n) '($rtn))))
             (cons '$ldf (cons body c)) ))

    ((eq (car e) 'let)
     (let ((m (cons (vars (cdr (cdr e))) n))
           (args (exprs (cdr (cdr e)))))
             (let ((body (comp (car (cdr e)) m '($rtn))))
                  (complis args n (cons '$ldf (cons body (cons '$ap c)))) )))

    ((eq (car e) 'letrec)
     (let ((m (cons (vars (cdr (cdr e))) n))
           (args (exprs (cdr (cdr e)))))
          (let ((body (comp (car (cdr e)) m '($rtn))))
               (cons '$dum (complis args m (cons '$ldf (cons body (cons '$rap c)))) ))))
    (t
     (complis (cdr e) n (comp (car e) n (cons '$ap c))) )))

(defun complis (e n c)
  (if (eq e nil) (cons '$ldc (cons nil c))
      (complis (cdr e) n (comp (car e) n (cons '$cons c)))))

(defun location (e n)
    (if (member e (car n)) (cons 0 (posn e (car n)))
        (incar (location e (cdr n)))))

(defun member (e n)
    (if (eq n nil) nil
    (if (eq e (car n)) t (member e (cdr n)))))

(defun posn (e n)
    (if (eq e (car n)) 0 (add 1 (posn e (cdr n)))))

(defun incar (l) (cons (add 1 (car l)) (cdr l)))

(defun vars (d)
  (if (eq d nil) nil
      (cons (car (car d)) (vars (cdr d)))))

(defun exprs (d)
  (if (eq d nil) nil
      (cons (cdr (car d)) (exprs (cdr d)))))

(setq add +)


(vm-run (compile (println '(let (sub a b) (a add 1 2) (b add 3 4)))))

(vm-run
  (compile
    (println
      '(letrec
           (fac 10)

           (fac lambda(n)
             (if (eq n 1) 1
               (mul n (fac (sub n 1))) ))))))



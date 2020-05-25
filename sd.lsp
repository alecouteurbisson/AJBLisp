;; subst : Exp * Val * Num --> Exp
;; subst(K, V, N) = K
;; subst(N, V, N') = V,              (N = N')
;; subst(N, V, N') = N,              (N < N')
;; subst(N + 1, V, N') = N,          (N >= N')
;; subst(lambda M, V, N)
;;   =  subst(M, bump(V, 0), N + 1)
;; subst((M M'), V, N)
;;   = (subst(M, V, N) subst(M', V, N))

 (defun subs(k v n)
   (cond
     ((symbolp k) k)
     ((numberp k)
      (if (eq k n) v n) )
     ((lambdap k) (subs (cadr k) (bump v 0) (+ n 1)))
     (t
      (cons (subs (car k) v n) (subs (cadr k) v n)) )))

;; bump : Exp * Nat --> Exp
;; bump(K, N) = K
;; bump(N, N') = N,                  (N <= N')
;; bump(N, N') = N + 1,              (N > N')
;; bump( M, N) =  bump(M, N + 1)
;; bump((M M'), N) = (bump(M, N) bump(M', N))
  (defun bump (k n)
    (cond
      ((symbolp k) k)
      ((numberp k)
       (if (<= k n) n (+ n 1)) )
      ((listp k)
       (list (bump (car k) n) (bump (cadr k) n)) )
      (t
       (bump k (+ n 1)) )))

(defun lambdap (x)
  (and (consp x) (eq (car x) lambda)) )

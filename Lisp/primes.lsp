;; Yup! Vectors seem to work OK.
;; As does for.
(setq primes (make-vector 1024 nil))

(for (i 2 31)
 (if (not (vref primes i))
     (for (j (+ i i) 1023 i) (vset primes j t)) ))


(for (i 1 1023) (if (not (vref primes i)) (print i :space)))

(println)

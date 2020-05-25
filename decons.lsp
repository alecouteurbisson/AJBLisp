;; decons will deconstruct a list using a pattern as a guide.
;; Quoted items in the pattern must match exactly in the list.
;; Symbols in the pattern are bound to the corresponding parts
;; of the list by an asscociation list that is returned upon
;; success.
;;
;; Failure results in an integer being returned:
;;  1 - list too big for pattern
;;  2 - quoted item not found
;;  3 - conflicting matches
;;  4 - bad pattern item

; > (decons '(x 'b (y 'j x . w)   z)
;           '(a  b (c  j a   e a) d) )
;
; = ((z . d) (w e a) (y . c) (x . a))

(defkey :fail)

(defun decons(pattern form)
  (catch :fail
    (let ((result)
         (tmp)

         (dc '(lambda (p f)
               (cond
                 ((null p)
                  (when f (throw :fail 1)))

                 ((and (consp p) (eq (car p) 'quote))
                  (unless (eq (cadr p) f)
                    (throw :fail 2) ))

                 ((symbolp p)
                  (setq tmp (cdrassoc p result))
                  (if (eq tmp :undefined)
                      (push (cons p f) result)
                      (when (not (equal tmp f))
                        (throw :fail 3) )))

                 ((consp p)
                  (dc (car p) (car f))
                  (dc (cdr p) (cdr f)) )

                 (t
                   (throw :fail 4) )))))
        (dc pattern form)
        result )))


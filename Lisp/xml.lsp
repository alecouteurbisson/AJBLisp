(setq tag car)
(setq attr cadr)
(setq content cddr)

(defun node x x)

(defun x-print(l)
  ;(println "Debug xprint " l)
  (if (atom l)
      (println l)
      (progn
        (if (atom (car l))
            (progn
              (print #\< (tag l))
              (for-each (a (attr l))
                (print :space (car a) #\= #\" (cdr a) #\") )
              (if (content l)
                (progn
                  (println #\>)
                  (for-each (itm (content l))
                    (x-print itm))
                  (println #\<#\/ (tag l) #\>) )
                (println #\/ #\>) ))
        (error 1 l "Bad xmldoc list")))))

(setq x (node 'screen '((x . 640) (y . 480))
          (node 'lines nil
            (node 'line '((n . 1)) "a line of text")
            (node 'line '((n . 2)) "another line of text")
            (node 'line '((n . 3)) "the last line of text") )))

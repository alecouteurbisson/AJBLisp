;; Test encode-time with random dates and times
;;
(setq md #(0 31 28 31 30 31 30 31 31 30 31 30 31))

(integer-format "%#3ld")

(setq errors 0)

(defun test-et nil
  (for (k 1 10000)
    (slet ((y (+ 1970 (random 68)))
           (m (+ 1 (random 12)))
           (d (+ 1 (random (vref md m))))
           (H (random 24))
           (M (random 60))
           (S (random 60))
           (it (encode-time y m d H M S))
           (dt (date-time it)))
          (print (list y m d H M S) " -> " dt)
          (if (or (<> (nth 0 dt) y)
                  (<> (nth 1 dt) m)
                  (<> (nth 2 dt) d)
                  (<> (nth 3 dt) H)
                  (<> (nth 4 dt) M)
                  (<> (nth 5 dt) S) )
              (print "ERROR" (inc errors)) )
          (println) )))

(logfile "time.txt")

; Only works for GMT
(gmt t)
(test-et)

(println errors " total errors")

(logfile)

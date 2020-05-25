(defun ls ((s . ".\\*"))
  (for-each (de (dir s t) ind (+ ind 1))
    ; File/Directory name
    (print-format "%-32s " (car de))
    ; Attributes
    (for-each (c (chars (cadr de)))
      (print (if (lower-casep c) "." c)) )
    ; Modified date
    (print-format "   %s\n" (time-string "%c" (nth 4 de))) ))



;;; A list sort
;;; Rather than go to a good reference on sorting I thought I'd
;;; invent my own!  Its based upon the well-known principle of
;;; merging sorted sub-lists.
;;; The algorithm is notable for the fact that the resulting
;;; sorted list is composed entirely of the cons nodes supplied
;;; in the input list!  It is therefore efficient in term of
;;; heap space use. It is not a stable sort and it destroys
;;; the argument list.
;;;
;;; Example calls are:
;;;
;;; (sort '(5 3 6 8 2 3 5 1))
;;; (sort '("miny" "eeny" "mo" "meeny") string>)
;;;
;;; or, using a user defined comparison
;;;
;;; (defun lgr (lgr~x lgr~y) (> (car lgr~x) (car lgr~y)))
;;; (sort '((3 a c)(1 d f)(2 h p)) lgr)

;;; Sort a list of items optionally supplying a greater-than 
;;; function for the list elements.  The default is >.
;;; 
(defun sort (l (sort~gt . >))
  (let ((len (length l)))
    (if (> len 2)
        (sort-aux  l len sort~gt)
        (sort-pair l len sort~gt) )))

;;; Macro version sets the argument to the sorted result
(defmacro msort (l (sort~gt . >))
  (set l (sort (eval l) (eval sort~gt))) )
  
;;; Sort a list of upto two items destructively
(defun sort-pair(l len sort~gt)
  (case len
    (0 nil)
    (1 l)
    (2 (if (sort~gt (car l) (cadr l))
           (prog1 (cdr l)
                  (rplacd (cdr l) l)
                  (rplacd l nil) )
           l ))))

;;; Sort more than two items
(defun sort-aux (l len sort~gt)
  ;; Slice the list destructively taking care to scan the
  ;; list once only
  (slet ((len1 (/ len 2))
         (len2 (- len len1))
         (l1   l)
         (l2   (nthcdr (- len1 1) l)) )
    ;; Slice just after l2 and set l2 to the second half
    (setq l2 (prog1 (cdr l2)
                    (rplacd l2 nil) ))
    ;; Sort and merge the halves                    
    (merge (sort l1 sort~gt) (sort l2 sort~gt) sort~gt) ))

;;; A destructive merge.  It rearranges the cons cells
;;; in the input lists to create the merged list
(defun merge (x y sort~gt)
  (slet ((merged '(nil))                     ; '(nil) is just a...
         (tail   merged))                    ; ...handle for the result...
    (loop
      ;; Deal with empty lists
      (while x (rplacd tail y) (cdr merged)) ; ...discarded here...
      (while y (rplacd tail x) (cdr merged)) ; ...or here
      ;; Move the cons node with the smaller car to the result
      (setq tail
            (cdr (rplacd tail
                         (if (sort~gt (car y) (car x))
                             (prog1 x (pop x))
                             (prog1 y (pop y)) )))))))

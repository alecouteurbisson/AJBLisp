(defmacro rename-vars (func)
  (let ((vars (cadr (eval func))))
       (for-each (var vars)
         (set func
	      (subst (free var) var (eval func))) )))

(defmacro minc (x) (set x (+ 1 (eval x))))

(rename-vars minc)

(pprint minc)

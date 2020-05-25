(setq *classes* nil
      *objects* nil)

(setq types '(::bool ::int ::decimal ::string ::date ::time ::datetime))

(for-each (t types) (defkey t))

(defun istype (type)
  (member type types) )

(defun class (cls)
  (unless (variablep cls) (error 1 cls "Invalid class name - must be a variable"))
  (push cls *classes*) )

(defun isclass (cls)
  (member cls *classes*) )

(defun add-property (type prop cls)
  (unless (istype type)(error 1 type "Not a type"))
  (unless (isclass cls) (error 1 cls "Not a class"))
  (when (assoc (prop cls)) (error 1 (cons prop cls) "Property exists"))
  (push (cons prop type) cls) )

;; Dates are represented by (::date y m d)
(defun isdate (d)
   (cond
     ((not (listp d)) nil)
     ((not (eq (car d) ::date)) nil)
     ((not (= (length d) 4)) nil)
     ((atom (errorset (apply encode-time (cdr d)))) nil)
     (t t) ))

;; Times are represented by (::time h m s)
(defun istime (d)
   (cond
     ((not (listp d)) nil)
     ((not (eq (car d) ::time)) nil)
     ((not (= (length d) 4)) nil)
     ((atom (errorset
                 (apply encode-time (append '(1970 1 1)
						        (cdr d) )))) nil)
     (t t) ))

;; Datetimes are represented by (::datetime y m d h m s)
(defun isdatetime (d)
   (cond
     ((not (listp d)) nil)
     ((not (eq (car d) ::datetime)) nil)
     ((not (= (length d) 7)) nil)
     ((atom (errorset
                 (apply encode-time (cdr d) ))) nil)
     (t t) ))

;;;;
;;;;  Regression test for the AJBLisp interpreter
;;;;

(logfile "testlog.txt" t)
(float-format "%#1.8lg")
(setq verbose nil)

;;; Allowed fuzz in floating results
(setq eps 1e-9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Call and Argument Passing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arguments ()
  (heading "Test function definitions and argument passing")

  (test (q1 a b c)               (a b c))
  (test (q2 a b)                 (a b))
  (test (qdefault 1)             (1 nil 5))
  (test (qdefault 1 2)           (1 2 5))
  (test (qdefault x y z)         (x y z))
  (test (elist 'a 'b 'c)         (a b c))
  (test (e2 'a 'b)               (a b))
  (test (edefault 1)             (1 nil 5))
  (test (edefault 1 2)           (1 2 5))
  (test (edefault 'x 'y 'z)      (x y z)) )

;;;;; Test function definitions
;;; Macro with list argument
(defmacro q1 args args)

;;; Macro with two args
(defmacro q2 (a b) (list a b))

;;; Macro with optional and defaulted args
(defmacro qdefault (a (b) (c . 5))
  (list a b c) )

;;; Lambda with list argument
(defun elist arg arg)

;;; Lambda with two args
(defun e2 (a b) (list a b))

;;; Lambda with optional and defaulted args
(defun edefault (a (b) (c . 5))
  (list a b c) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic List Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun basic-list ()
  (heading "Test some basic list operations")
  (test (car '((a b) (c d)))     (a b))
  (test (cdr '((a b) (c d)))     ((c d)))
  (test (caar '((a b) (c d)))    a)
  (test (cdar '((a b) (c d)))    (b))
  (test (cadr '((a b) c))        c)
  (test (cddr '((a b) c d))      (d))
  (test (cons 'a '(c d))         (a c d))
  (test (cons '(a b) '((c d)))   ((a b)(c d)))
  (test (cons 'a 'b)             (a . b)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditional Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun conditionals ()
  (heading "Test conditionals")
  (test (if t 'a 'b)                    a)
  (test (if nil 'a 'b)                  b)
  (test (if t 'a)                       a)
  (test (if nil 'a)                     nil)
  (test (cond (t))                      t)
  (test (cond (nil))                    nil)
  (test (cond (t 'a))                   a)
  (test (cond (nil 'a))                 nil)
  (test (cond ((atom 'a) 'a) (t 'b))    a)
  (test (cond ((listp 'a) 'a) (t 'b))   b) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-ops ()
  (heading "Test list operations")
  (test (nth 0 '(a b c))                a)
  (test (nth 2 '(a b c))                c)
  (test (nthcdr 0 '(a b c))             (a b c))
  (test (nthcdr 2 '(a b c))             (c))
  (test (append)                        nil)
  (test (append nil nil nil nil)        nil)
  (test (append '(a b) nil)             (a b))
  (test (append nil '(a b))             (a b))
  (test (append '(a b) '(c d))          (a b c d))
  (test (append '((a b)) '((c d)))      ((a b)(c d)))
  (test (append '((a)) '(b) '((c d)))   ((a) b (c d)))
  (test (append nil '(a b)
                nil '((c d)) nil)       (a b (c d)))
  (test (mapc 'list '(a b c))           ((a)(b)(c)))
  (test (mapc '- '(1 2 3))              (-1 -2 -3))
  (test (mapc (collect) nil)            nil)
  (test (delete 'a '(a b a c a))        (b c))
  (test (delete 'a '(a a a a a))        nil)
  (test (length '(a b c))               3)
  (test (member 'c '(a b c d e))        (c d e))
  (test (member 'x '(a b c d e))        nil)
  (test (filter numberp
                '(a 1 2 b c 3 d) )      (1 2 3) )
  (test (filter numberp
                '(a 1 2 b c 3 d) nil )  (a b c d) )
  (test (reverse '(a b c d e))          (e d c b a)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vector-ops ()
  (heading "Test vector operations")
  (let ((v (vector '(a b c d e))))
    (test (make-vector 3 'a)            #(a a a))
    (test (vref v 2)                    c)
    (vset v 2 'x)
    (test (vref v 2)                    x)
    (test (vector "Hello")              #(#\H #\e #\l #\l #\o))
    (test (length v)                    5)
    (test (reverse v)                   #(e d x b a))
    (test (reverse v 3)                 #(x b a)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equality and identity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun equality ()
  (test (eq 'a 'a)                      t)
  (test (eq 'a (free a))                nil)
  (test (eq 1000 1000)                  nil)
  (test (eq 1 1.0)                      nil)
  (test (eq "a" "a")                    nil)
  (test (eq '(a b c) '(a b c))          nil)
  (test (eq #(a b c) #(a b c))          nil)
  (test (eql 'a 'a)                     t)
  (test (eql 'a (free a))               nil)
  (test (eql 1 1)                       t)
  (test (eql 1 1.0)                     nil)
  (test (eql "a" "a")                   t)
  (test (eql '(a b c) '(a b c))         nil)
  (test (eql #(a b c) #(a b c))         nil)
  (test (equal 'a 'a)                   t)
  (test (equal 'a (free a))             nil)
  (test (equal 1 1)                     t)
  (test (equal 1 1.0)                   nil)
  (test (equal "a" "a")                 t)
  (test (equal '(a b c) '(a b c))       t)
  (test (equal #(a b c) #(a b c))       t) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heap, Stack and GC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun heap-stack ()
  (heading "Hammer the stack and heap")
  (test (tak 18 12 6) 7) )

;;; A highly recursive function
(defun tak (x y z)
  (if (>= y x) z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boolean ()
  (heading "Test the boolean predicates")
  (test (not t)         nil)
  (test (not nil)       t)
  (test (eq nil nil)    t)
  (test (eq t nil)      nil)
  (test (eq nil t)      nil)
  (test (eq t t)        t)
  (test (and)           t)
  (test (and nil nil)   nil)
  (test (and t nil)     nil)
  (test (and nil t)     nil)
  (test (and t t)       t)
  (test (or)            nil)
  (test (or nil nil)    nil)
  (test (or t nil)      t)
  (test (or nil t)      t)
  (test (or t t)        t) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logical Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logical ()
  (heading "Test bitwise logical operation")
  (test (bnot 0xf0f0f0f0)    0x0f0f0f0f)
  (test (bnot 0x0f0f0f0f)    0xf0f0f0f0)
  (test (band 0x00ff 0x0f0f) 0x000f)
  (test (bor  0x00ff 0x0f0f) 0x0fff)
  (test (bxor 0x00ff 0x0f0f) 0x0ff0)
  (test (bset 0 0x0)         0x1)
  (test (bset 31 0x0)        0x80000000)
  (test (bclr 0 0xffffffff)  0xfffffffe)
  (test (bclr 31 0xffffffff) 0x7fffffff)
  (test (btst 0 0xfffffffe)  nil)
  (test (btst 31 0x7fffffff) nil)
  (test (btst 0 0x00000001)  t)
  (test (btst 31 0x80000000) t)
  (test (bcnt 0x0)           0)
  (test (bcnt 0xaaaaaaaa)    16)
  (test (bcnt 0x55555555)    16)
  (test (bcnt 0xffffffff)    32) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numeric Ordering Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun num-order ()
  (heading "Test numeric ordering operations")
  (test (=  0 0)         t)
  (test (>  0 0)         nil)
  (test (<  0 0)         nil)
  (test (>= 0 0)         t)
  (test (<= 0 0)         t)
  (test (<> 0 0)         nil)

  (test (=  1 1)         t)
  (test (>  1 1)         nil)
  (test (<  1 1)         nil)
  (test (>= 1 1)         t)
  (test (<= 1 1)         t)
  (test (<> 1 1)         nil)

  (test (=  1 0)         nil)
  (test (>  1 0)         t)
  (test (<  1 0)         nil)
  (test (>= 1 0)         t)
  (test (<= 1 0)         nil)
  (test (<> 1 0)         t)

  (test (=  0 1)         nil)
  (test (>  0 1)         nil)
  (test (<  0 1)         t)
  (test (>= 0 1)         nil)
  (test (<= 0 1)         t)
  (test (<> 0 1)         t)

  (test (=  -1 -1)       t)
  (test (>  -1 -1)       nil)
  (test (<  -1 -1)       nil)
  (test (>= -1 -1)       t)
  (test (<= -1 -1)       t)
  (test (<> -1 -1)       nil)

  (test (=  -1 0)        nil)
  (test (>  -1 0)        nil)
  (test (<  -1 0)        t)
  (test (>= -1 0)        nil)
  (test (<= -1 0)        t)
  (test (<> -1 0)        t)

  (test (=  0 -1)        nil)
  (test (>  0 -1)        t)
  (test (<  0 -1)        nil)
  (test (>= 0 -1)        t)
  (test (<= 0 -1)        nil)
  (test (<> 0 -1)        t)

  (test (=  0.0 0.0)     t)
  (test (>  0.0 0.0)     nil)
  (test (<  0.0 0.0)     nil)
  (test (>= 0.0 0.0)     t)
  (test (<= 0.0 0.0)     t)
  (test (<> 0.0 0.0)     nil)

  (test (=  1.0 1.0)     t)
  (test (>  1.0 1.0)     nil)
  (test (<  1.0 1.0)     nil)
  (test (>= 1.0 1.0)     t)
  (test (<= 1.0 1.0)     t)
  (test (<> 1.0 1.0)     nil)

  (test (=  1.0 0.0)     nil)
  (test (>  1.0 0.0)     t)
  (test (<  1.0 0.0)     nil)
  (test (>= 1.0 0.0)     t)
  (test (<= 1.0 0.0)     nil)
  (test (<> 1.0 0.0)     t)

  (test (=  0.0 1.0)     nil)
  (test (>  0.0 1.0)     nil)
  (test (<  0.0 1.0)     t)
  (test (>= 0.0 1.0)     nil)
  (test (<= 0.0 1.0)     t)
  (test (<> 0.0 1.0)     t)

  (test (=  -1.0 -1.0)   t)
  (test (>  -1.0 -1.0)   nil)
  (test (<  -1.0 -1.0)   nil)
  (test (>= -1.0 -1.0)   t)
  (test (<= -1.0 -1.0)   t)
  (test (<> -1.0 -1.0)   nil)

  (test (=  -1.0 0.0)    nil)
  (test (>  -1.0 0.0)    nil)
  (test (<  -1.0 0.0)    t)
  (test (>= -1.0 0.0)    nil)
  (test (<= -1.0 0.0)    t)
  (test (<> -1.0 0.0)    t)

  (test (=  0.0 -1.0)    nil)
  (test (>  0.0 -1.0)    t)
  (test (<  0.0 -1.0)    nil)
  (test (>= 0.0 -1.0)    t)
  (test (<= 0.0 -1.0)    nil)
  (test (<> 0.0 -1.0)    t) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character Ordering Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun char-order ()
  (heading "Test character ordering operations")
  (test (char=  #\b #\b)            t)
  (test (char=  #\b #\a)            nil)
  (test (char<> #\b #\b)            nil)
  (test (char<> #\b #\a)            t)
  (test (char>  #\b #\b)            nil)
  (test (char>  #\a #\b)            nil)
  (test (char>  #\c #\b)            t)
  (test (char<  #\b #\b)            nil)
  (test (char<  #\a #\b)            t)
  (test (char<  #\c #\b)            nil)
  (test (char>= #\b #\b)            t)
  (test (char>= #\a #\b)            nil)
  (test (char>= #\c #\b)            t)
  (test (char<= #\b #\b)            t)
  (test (char<= #\a #\b)            t)
  (test (char<= #\c #\b)            nil)

  ; Check :nocase flag
  (test (char=  #\B #\b)            nil)
  (test (char=  #\B #\b :nocase)    t)
  (test (char<> #\B #\b)            t)
  (test (char<> #\B #\b :nocase)    nil) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Ordering Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-order ()
  (heading "Test string ordering operations")
  (test (string=  "abc" "abc")            t)
  (test (string=  "abc" "abd")            nil)
  (test (string<> "abc" "abc")            nil)
  (test (string<> "abc" "abd")            t)
  (test (string>  "abc" "abc")            nil)
  (test (string>  "aba" "abc")            nil)
  (test (string>  "abd" "abc")            t)
  (test (string<  "abc" "abc")            nil)
  (test (string<  "aba" "abc")            t)
  (test (string<  "abd" "abc")            nil)
  (test (string>= "abc" "abc")            t)
  (test (string>= "aba" "abc")            nil)
  (test (string>= "abd" "abc")            t)
  (test (string<= "abc" "abc")            t)
  (test (string<= "aba" "abc")            t)
  (test (string<= "abd" "abc")            nil)

  ; Check :nocase flag
  (test (string=  "abc" "ABC")            nil)
  (test (string=  "abc" "ABC" :nocase)    t)
  (test (string<> "abc" "ABC")            t)
  (test (string<> "abc" "ABC" :nocase)    nil) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-ops ()
  (heading "Test other string & character operations")
  (test (upper-case "a1Ab*c")       "A1AB*C")
  (test (lower-case "a1Ab*c")       "a1ab*c")
  (test (upper-case #\A)            #\A)
  (test (lower-case #\A)            #\a)
  (test (upper-case #\a)            #\A)
  (test (lower-case #\a)            #\a)
  (test (upper-case #\3)            #\3)
  (test (lower-case #\4)            #\4)
  (test (string 1)                  "1")
  (test (string 1.0)                "1.0000000")
  (test (string "abc")              "abc")
  (test (string '(#\a #\b #\c))     "abc")
  (test (string #(#\a #\b #\c))     "abc")
  (test (length "Hello")            5)
  (test (upper-casep #\A)           t)
  (test (lower-casep #\A)           nil)
  (test (upper-casep #\a)           nil)
  (test (lower-casep #\a)           t)
  (test (chars "Hello")             (#\H #\e #\l #\l #\o))
  (test (concat)                    "")
  (test (concat "a" "b" "c" "d")    "abcd")
  (test (length "")                 0)
  (test (length "abcd")             4) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type-ops ()
  (heading "Test type predicates")
  (slet
    ((vect #(2 2))
     (types (list nil 'a '(a) 0 1 0.0 1.0 5 5.0 -1 -1.0 "Hi" vect list let :stdin :create ':err_num_args)) )
    ;
    ;                               nil sym  cons 0    1    0.0  1.0  5    5.0  -1   -1.0  "Hi" vect subr fsubr file key const
    (test (test-types atom)        (t   t    nil  t    t    t    t    t    t    t    t     t    nil  t    t     t    t   t   ))
    (test (test-types zerop)       (nil nil  nil  t    nil  t    nil  nil  nil  nil  nil   nil  nil  nil  nil   nil  nil nil ))
    (test (test-types onep)        (nil nil  nil  nil  t    nil  t    nil  nil  nil  nil   nil  nil  nil  nil   nil  nil nil ))
    (test (test-types minusp)      (nil nil  nil  nil  nil  nil  nil  nil  nil  t    t     nil  nil  nil  nil   nil  nil nil ))
    (test (test-types plusp)       (nil nil  nil  t    t    t    t    t    t    nil  nil   nil  nil  nil  nil   nil  nil nil ))
    (test (test-types consp)       (nil nil  t    nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  nil  nil   nil  nil nil ))
    (test (test-types symbolp)     (nil t    nil  nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  nil  nil   nil  t   t   ))
    (test (test-types numberp)     (nil nil  nil  t    t    t    t    t    t    t    t     nil  nil  nil  nil   nil  nil nil ))
    (test (test-types integerp)    (nil nil  nil  t    t    nil  nil  t    nil  t    nil   nil  nil  nil  nil   nil  nil nil ))
    (test (test-types floatp)      (nil nil  nil  nil  nil  t    t    nil  t    nil  t     nil  nil  nil  nil   nil  nil nil ))
    (test (test-types stringp)     (nil nil  nil  nil  nil  nil  nil  nil  nil  nil  nil   t    nil  nil  nil   nil  nil nil ))
    (test (test-types listp)       (t   nil  t    nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  nil  nil   nil  nil nil ))
    (test (test-types vectorp)     (nil nil  nil  nil  nil  nil  nil  nil  nil  nil  nil   nil  t    nil  nil   nil  nil nil ))
    (test (test-types subrp)       (nil nil  nil  nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  t    nil   nil  nil nil ))
    (test (test-types fsubrp)      (nil nil  nil  nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  nil  t     nil  nil nil ))
    (test (test-types filep)       (nil nil  nil  nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  nil  nil   t    nil nil ))
    (test (test-types keyp)        (nil nil  nil  nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  nil  nil   nil  t   nil ))
    (test (test-types constantp)   (nil nil  nil  nil  nil  nil  nil  nil  nil  nil  nil   nil  nil  nil  nil   nil  t   t   ))

    (test (mapc typeof types)       (:symbol :symbol :cons :integer :integer :float :float :integer :float
                                     :integer :float :string :vector :subrl :fsubrl :file :key :constant) )))

;;; Utility function for type testing
(defun test-types(pred)
  (mapc pred types) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flow Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flow-control ()
  (heading "Flow control")

  (test (test-cond 5)           t)
  (test (test-cond 6)           "number")
  (test (test-cond a)           "atom")
  (test (test-cond #(a b c))    "fall-through")

  (test (test-case 1)           "one")
  (test (test-case 2)           "two-four")
  (test (test-case 3)           "two-four")
  (test (test-case 4)           "two-four")
  (test (test-case 5)           "no match")
  (test (test-case nil)         "nil")

  (test (test-loop 1)           "a=5")
  (test (test-loop 5)           "a=7")
  (test (test-loop 7)           "a>=10") )

(defmacro test-cond (a)
  (cond
    ((eql a 5))
    ((numberp a) "number")
    ((atom a)    "atom")
    (t           "fall-through") ))

(defmacro test-case (c)
  (case c
    (1          "one")
    ((2 3 4)    "two-four")
    (nil        "nil")
    (:else      "no match") ))

(defmacro test-loop (a)
  (loop
    (inc a)
    (while (< a 10)  "a>=10")
    (until (= a 5)   "a=5")
    (when  (= a 7)   (break "a=7")) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-io ()
  (heading "File I/O")
  (let
    ((fac '(lambda (x) (if (< x 2) 1 (* x (fac (- x 1))))))
     (fac2)
     (file) )

    (test (fac 6) 720)

    (setq file (open-file "test-fac.lsp" :create))
    (if file
        (writeln file fac) )
    (test (close-file file) t)

    (setq file (open-file "test-fac.lsp" :read))
    (if file
        (progn
          (setq fac2 (read file))
          (test (fac2 6) 720) ))
    (test (close-file file) t) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arithmetic ()
  (heading "Test arithmetic")

  (test (+)              0)
  (test (+ 2)            2)
  (test (+ 2.0)          2.0)
  (test (+ 1 2 3 4 5)    15)
  (test (+ 1 2 3.0 4 5)  15.0)
  (test (*)              1)
  (test (* 2)            2)
  (test (* 2.0)          2.0)
  (test (* 1 2 3 4 5)    120)
  (test (* 1 2 3.0 4 5)  120.0)
  (test (- 1)           -1)
  (test (- 1.0)         -1.0)
  (test (- 6 4)          2)
  (test (- 6 4.0)        2.0)
  (test (/ 12 3)         4)
  (test (/ 12.0 3)       4.0)
  (test (/ 10)           0)
  (test (/ 10.0)         0.1)
  (test (rem 54 10)      4)
  (test (abs 1)          1)
  (test (abs -1)         1)
  (test (abs 1.0)        1.0)
  (test (abs -1.0)       1.0)
  (test (floor 5.678)    5)
  (test (floor -5.678)  -6)
  (test (ceil 5.678)     6)
  (test (ceil -5.678)   -5)
  (test (float 5)        5.0)

  (setq a 5)
  (test (inc a)          6)
  (test a                6)
  (test (dec a)          5)
  (test a                5) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun maths ()
  (heading "Maths functions")

  ;; Just check that we are calling the correct functions
  ;; we are not checking the C library here...
  (test-fuzz (sqrt 256)       16.0)
  (test-fuzz (sqrt 256.0)     16.0)
  (test-fuzz (power 2.0 5.0)  32.0)
  (test-fuzz (exp 1.0)         2.71828182846)
  (test-fuzz (log 3.0)         0.47712125472)
  (test-fuzz (ln  3.0)         1.09861228867)
  (test-fuzz (sin 1.0)         0.841470984808)
  (test-fuzz (sin -1.0)       -0.841470984808)
  (test-fuzz (cos 1.0)         0.540302305868)
  (test-fuzz (cos -1.0)        0.540302305868)
  (test-fuzz (tan 1.0)         1.55740772465)
  (test-fuzz (tan -1.0)       -1.55740772465)
  (test-fuzz (asin 0.5)        0.523598775598)
  (test-fuzz (asin -0.5)      -0.523598775598)
  (test-fuzz (acos 0.5)        1.0471975512)
  (test-fuzz (acos -0.5)       2.09439510239)
  (test-fuzz (atan 0.5)        0.463647609001)
  (test-fuzz (atan -0.5)      -0.463647609001)
  (test-fuzz (atan 2.0 1.0)    0.463647609001)
  (test-fuzz (atan -2.0 1.0)   2.67794504459)
  (test-fuzz (sinh 0.5)        0.521095305494)
  (test-fuzz (sinh -0.5)      -0.521095305494)
  (test-fuzz (cosh 0.5)        1.12762596521)
  (test-fuzz (cosh -0.5)       1.12762596521)
  (test-fuzz (tanh 0.5)        0.46211715726)
  (test-fuzz (tanh -0.5)      -0.46211715726)
  (test-fuzz (asinh 0.5)       0.48121182506)
  (test-fuzz (asinh -0.5)     -0.48121182506)
  (test-fuzz (acosh 1.5)       0.962423650119)
  (test-fuzz (atanh 0.5)       0.549306144334)
  (test-fuzz (atanh -0.5)     -0.549306144334) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maths Exceptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun maths-except ()
  (heading "Maths exceptions")
  ; The largest representable positive integer
  (setq ibig 0x7FFFFFFF)

  (expect-error (+ ibig 1)        :err_m_ovrf)
  (expect-error (- (- ibig) 2)    :err_m_ovrf)
  (expect-error (* 70000 70000)   :err_m_ovrf)
  (expect-error (/ 0)             :err_m_divz)
  (expect-error (/ 0.0)           :err_m_divz)
  (expect-error (sqrt -1)         :err_m_dom)
  (expect-error (log -1)          :err_m_dom)
  (expect-error (ln -1)           :err_m_dom)
  (expect-error (exp 1e10)        :err_m_dom)
  (expect-error (sin 1e10)        :err_m_dom)
  (expect-error (cos 1e10)        :err_m_dom)
  (expect-error (tan 1e10)        :err_m_dom)
  (expect-error (asin 2.0)        :err_m_dom)
  (expect-error (acos 2.0)        :err_m_dom)
  (expect-error (acosh 0.0)       :err_m_dom)
  (expect-error (power -1 0.5)    :err_m_dom)
  (expect-error (ceil 1e99)       :err_m_inv)
  (expect-error (floor 1e99)      :err_m_inv)
  (expect-error (integer 1e99)    :err_m_inv) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non Local Exits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun non-local-exit ()
  (heading "Non-local exit")
  (test (test-nle nil)   (payload))
  (test a                guarded)
  (test (test-nle t)     1)
  (test a                guarded) )

(defkey :ball)

(defun test-nle (tst)
  (setq a 'init)
  (errorset
    (catch :ball
      (collect)
      (guard
        (if tst
          (error 1)
          (throw :ball 'payload) )
        (setq a 'guarded) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date and Time Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun date-and-time ()
  (local-time nil)
  (test (date 0)                       (1970 1 1))
  (test (date 0x7FFFFFFF)              (2038 1 19))
  (test (time 0)                       (0 0 0))
  (test (time 0x7FFFFFFF)              (3 14 7))
  (test (date-time 0)                  (1970 1 1 0 0 0 4 0 nil))
  (test (date-time 0x7FFFFFFF)         (2038 1 19 3 14 7 2 18 nil))
  (test (encode-time 2038 1 19 3 14 7) 0x7FFFFFFF)
  (test (encode-time 1970 1 1 0 0 0)   0)
  (local-time t) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun application ()
  (heading "Finally, run an application")
  (when (eq test-parse :undefined)
    (load "test-parse.lsp"))
  ;; Just check that it runs without error
  (test (test-parse expressions verbose) nil) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run the tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-all-tests nil
  (let
    ((failures 0)
    (failed nil) )

    ;; Run tests
    (arguments)
    (basic-list)
    (conditionals)
    (list-ops)
    (vector-ops)
    (equality)
    (heap-stack)
    (boolean)
    (logical)
    (num-order)
    (char-order)
    (string-order)
    (string-ops)
    (type-ops)
    (flow-control)
    (file-io)
    (arithmetic)
    (maths)
    (maths-except)
    (non-local-exit)
    (heap-stack)
    (date-and-time)
    (application)

    ;; Report failures

    (loop (while failed)
          (println (pop failed)) )
    failures ))

(defun heading (x)
  (if verbose
      (progn
        (println "###############################################################")
        (println "## " x)
        (println "###############################################################") )))

;;; General purpose test function - evaluates expr and compares with result
(defmacro test(expr expected)
  (let
    ((result (errorset (eval expr))))
    (cond
      ((eql result :err_user) (error :err_user))
      ((atom result)
       (println "**** " expr " failed with exception " result)
       (setq failed (cons (list expr " failed with exception " result)
                    failed) ))

      ((equal (car result) expected)
       (if verbose (println expr " = " expected)) )

      (t
        (println "**** " expr " != " expected " = " (car result))
        (setq failures (+ failures 1))
        (setq failed (cons (list expr " != " expected " = " (car result)) failed)) ))))

;;; General purpose maths test function - evaluates expr and compares with result allowing
;;; a little fuzziness in the answer
(defmacro test-fuzz(expr expected)
  (let
    ((result (errorset (eval expr))))
    (cond
      ((eql result :err_user) (error :err_user))
      ((atom result)
       (println "**** " expr " failed with exception " result)
       (setq failed (cons (list expr " failed with exception " result)
                    failed) ))

      ((= (car result) expected eps)
       (if verbose (println expr " = " expected)) )

      (t
        (println "**** " expr " != " expected " = " (car result))
        (setq failures (+ failures 1))
        (setq failed (cons (list expr " != " expected " = " (car result)) failed)) ))))

(defmacro expect-error(expr err)
  (setq err (eval err))
  (let ((result (errorset (eval expr))))
    (cond
      ((eql result :err_user) (error :err_user))
      ((eql result err)
       (if verbose (println "Exception " err " properly caught in " expr)) )
      ((numberp result)
       (if verbose (println "Wrong exception [" result "] caught in "
                            expr ", should be [" err "]" ))
       (setq failed
         (cons (list "Wrong exception [" result "] caught in "
                     expr ", should be [" err "]")
               failed )))
      (t
       (println "**** Failed to trap exception in " expr)
       (setq failures (+ 1 failures))
       (setq failed (cons (list "Exception not caught in " expr) failed)) ))))

(defun timed-run (rep)
  (statistics 0)
  (let ((start (now)) (elapsed) (end) (stats))
       (for (k 1 rep)
            (status (concat "Pass " (string k) ": " (string (run-all-tests)) " failures")) )

       (setq end (now))
       (setq elapsed (- end start))
       (setq stats (statistics))

       (status "Tests completed")
       (println)
       (println "*************   Summary   *************")
       (println "Elapsed time for " rep " tests = " elapsed " Seconds")
       (println "Eval called " (nth 0 stats) " times")
       (println "GC called " (nth 1 stats) " times")
       (println (nth 2 stats) " Cons nodes created")
       (println "Mean time per Eval = "
                (* 10.0e6 elapsed (/ (float (car stats))))
                " uSeconds" )))

(timed-run 1)

(logfile)
nil


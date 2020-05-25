;; A simple virtual stack machine in lisp
;;
;;

;; Memory
(defconstant progsz 0x20)
(setq program (make-vector progsz 0))

(defconstant stacksz 0x20)
(setq stack (make-vector stacksz 0))

(defconstant datasz 0x20)
(setq data (make-vector datasz 0))

;; Registers
;; SP   Stack Pointer
;; BP   Base Pointer
;; PC   Program Counter
;; NA   Number of arguments

;; Control functions

(defun reset ()
  (setq SP 0)
  (setq PC 0)
  (setq BP 0)
  (setq NA 0) )

(defun run ()
  (reset)
  (loop
    (setq INST (vref program PC))
    (eval INST)
    (show INST)
    (inc PC) ))

(defun show (instruction)
  (println PC instruction)
  (for (s 0 (- SP 1))
    (print (vref stack s) " " ))
  (println) )

;; Addressing modes

(defun TOS ()
  (vref stack (- SP 1)) )

(defun setTOS (x)
  (vset stack (- SP 1) x) )

;; Instructions

(defun vm_goto (a)
  (setq PC (- a 1)) )

(defun vm_branch (a)
  (setq PC (+ PC a)) )

(defun vm_goto_if_0 (a)
  (if (eql (vm_pop) 0)
      (setq PC (- a 1)) ))

(defun vm_branch_if_0 (a)
  (if (eql (vm_pop) 0)
      (setq PC (+ PC a)) ))

(defun vm_push (x)
  (vset stack SP x)
  (setq SP (+ SP 1))
  (if (= SP stacksz)
      (error 1 :none "Out of VM Stack") ))

(defun vm_pop ()
  (setq SP (- SP 1))
  (if (< SP 0)
      (error 1 :none "VM stack underflow") )
  (vref stack SP) )

(defun vm_dup ()
  (vm_push (TOS)) )

(defun vm_fetch ()
  (setq temp (vm_pop))
  (vm_push (vref data temp)) )

(defun vm_store ()
  (setq temp (vm_pop))
  (vset data temp (vm_pop)) )

(defun vm_call (n)
  ; Get destination
  (setq temp (vm_pop))
  ; Save number of args
  (vm_push NA)
  (setq NA n)
  (vm_push PC)
  (setq PC temp)
  ; Create frame
  (vm_push BP)
  (setq BP SP) )

(defun vm_return (n)
  ; Restore old environment
  (setq temp SP)
  (setq SP BP)
  (setq BP (vm_pop))
  (setq PC (vm_pop))
  (setq NA (vm_pop))
  ; Copy n returned values from top of old stack
  (for (i 0 (- n 1))
    (setq temp (- temp 1))
    (vm_push (vref stack temp)) ))

(defun vm_neg ()
  (setTOS (- (TOS))) )

(defun vm_add ()
  (setq temp (vm_pop))
  (setTOS (+ (TOS) temp)) )

(defun vm_sub ()
  (setq temp (vm_pop))
  (setTOS (- (TOS) temp)) )

(defun vm_not ()
  (setTOS (bnot (TOS))) )

(defun vm_and ()
  (setq temp (vm_pop))
  (setTOS (band (TOS) temp)) )

(defun vm_or ()
  (setq temp (vm_pop))
  (setTOS (bor (TOS) temp)) )

(defun vm_xor ()
  (setq temp (vm_pop))
  (setTOS (bxor (TOS) temp)) )

(defun vm_halt ()
  (break) )

;; Test it!
(defmacro write-program p
  (setq PC 0)
  (loop
    (while p)
    (vset program PC (pop p))
    (inc PC) ))

;; A simple program to sum the first n whole numbers
;; Input is via data memory 6 and the result is returned
;; in data memory 5
;;
(write-program
 (vm_push 0)          ;; Zero output total
 (vm_push 5)
 (vm_store)
 (vm_push 6)          ;; Get input
 (vm_fetch)
 (vm_dup)             ;; Test for end of loop
 (vm_goto_if_0 16)
 (vm_dup)             ;; Add counter to output
 (vm_push 5)
 (vm_fetch)
 (vm_add)
 (vm_push 5)
 (vm_store)
 (vm_push 1)          ;; Decrement counter
 (vm_sub)
 (vm_goto 5)          ;; Loop back
 (vm_pop)
 (vm_halt) )

;; Some input
(vset data 6 4)

;; Crank the handle...
(reset)
(run)

;; Check that we got an output
(print "Output = " (vref data 5))


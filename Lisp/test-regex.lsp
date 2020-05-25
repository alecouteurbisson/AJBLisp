(load "regex.lsp")

;;; Print an nfa in a sort of readable way
(defun print-nfa nil
  (println)
  (header "NFA")
  (for-each (state *nfa-states* index)
    (while state)
    (print-format "nfa %3d = %w\n" index state) ))

;;; Print a dfa in a sort of readable way
(defun print-dfa nil
  (println)
  (header "DFA")
  (for-each (state *dfa-states* index)
    (when (> index 0) (while state))
    (print-format "state %3d = %a\n" index state)
    (print-format "trans %3d = %w\n" index (vref *dfa-trans* index)) ))

(defun header (txt)
  (println)
  (println "************************************************************************")
  (println "**  " txt)
  (println "************************************************************************") )

;;; Try it all out
(logfile "debug.txt")

;;; Clear the decks
(init-regex)

(header "COMPILE REGEX")
;;; Create some macros to make things easier (and to check that they work!)
(define-regex digits (+ (- #\0 #\9)))
(define-regex sign (? (| #\+ #\-)))

;;; Compile some regexes to test
(compile-regex space     (+ (| #\^32 #\^M #\^J #\^I)))
(compile-regex kw_if     "if")
(compile-regex kw_exit   "exit")
(compile-regex kw_class  "class")
(compile-regex kw_exit1  "exit1")
(compile-regex intnum    (sign digits))
(compile-regex fpnum     (sign digits #\. (? digits)(? (| #\e #\E) sign digits)))
(compile-regex widget    ((* "a") (| (? "ab") "c") "d"))

;;; Print the NFA table
(print-nfa)

;;; Now compile the NFA to a DFA
(nfa-dfa)

;;; Print the DFA tables
(print-dfa)

;;; Get rid of the evidence
(cleanup)

;;; Test macro to throw a string at the DFA and print the parse
(defmacro test (x)
  (print-format "(parsing %w)  ->\n" x)
  (print-format "    %w\n" (run-dfa x)) )


;;; Throw some data at the DFA for parsing
(header "TEST")
(test "1")
(test ".1")
(test "+1")
(test "-1")
(test "1.5")
(test "+1.5")
(test "-1.5")
(test "12.3e-4")
(test "12.E4")
(test "12e4")
(test "12e")
(test "123456.35265")
(test "+123456.35265")
(test "-123456.35265")
(test "12+3456.35265")
(test "123 456.35 2+65")
(test "123456.352.65")
(test "123a456.35265")
(test "1234 56.35265.")
(test "exit    1234")
(test "exit11")
(test "exit21")
(test "frobozz 1234")
(test "12+13.5-2")
(test "aaaacd")
(test "aaaabd")
(test "aaabcd")
(test "abd")
(test "bd")
(test "abcd")
(test "cd")

;;; Close the logfile
(logfile)

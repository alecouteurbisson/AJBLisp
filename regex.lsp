;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regular expressions in AJBLisp
;;; A J Le Couteur Bisson - Nov 2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regular expressions
;;; Input syntax
;;; #\a         A terminal character
;;; x           A non-terminal symbol
;;; (a b)       Succession
;;; "abc"       Succession
;;; (* a)       Kleene star
;;; (| a b c)   Alternation
;;; (+ a)       One or more
;;; (? a)       Optional
;;; (- "a" "z") A range of characters
;;;
;;; (define-macro digit (- #\0 #\9))
;;; (define-regex integer ((? (| #\+ #\-)) (+ digit))
;;;
;;; This regex compiler converts all input regexes to NFAs and then converts
;;; these NFAs to a DFA that may be used for parsing. NFA and DFA states are
;;; stored in vectors so that each state corresponds to a vector index.
;;;
;;; The valid elements of vectors *nfa-states* and *dfa-trans* are a list of
;;; edges and terminal node markers.  Edges are represented by a cons of
;;; the character eaten and the next state index.  Terminal node markers are
;;; represented by a cons of the key :final and the parsed tokens regex name.
;;;
;;; e.g. a single entry in *nfa-states* might be:
;;;      ((#\: . 3)(#\@ . 14)(:eps . 5)(:final . doofer))
;;; The transition via :eps is an epsilon edge.
;;;
;;; Likewise, a single entry in *dfa-trans* might be
;;;      ((#\a . 8)(#\b . 4)(#\, . 13)(:final . widget)(:final thingummy))
;;; Note that there may be multiple terminal node markers here ordered by their
;;; order of compilation.  The first one normally takes priority.
;;;
;;; The vector *dfa-states* contains the set of nfa states covered by each of the
;;; *dfa-states* and may be discarded after compilation, as may the *nfa-states*
;;; vector.
;;;
;;; To parse an input using the generated dfa simply start at state 1
;;; and follow edges with each input character until no valid edges exist
;;; or a specific terminator character has been seen.
;;; If the present state contains a :final entry then you have successfully
;;; parsed the non-terminal given by the cdr of the first :final entry
;;; There will typically be other valid parses of the incomplete input so
;;; it is incorrect to wait for the first :final state to be seen.
;;; e.g. "123.45" will parse correctly at every step in a numeric value parser
;;;

(defconstant MAX_STATES 100)  ; Table size
(defkey :eps)                 ; Epsilon edge
(defkey :final)               ; Final state
(defkey :fail)                ; Parse failure throw tag
(defkey :eol)                 ; End of line (failure location)

;;; Initialise everything for a new regex
(defun init-regex nil
  (setq *nfa-states*     (make-vector MAX_STATES)
        *dfa-states*     (make-vector MAX_STATES)
        *dfa-trans*      (make-vector MAX_STATES)
        *macros*         nil
        *next-nfa-state* 1 ) )

;;; Create a new production
;;; Multiple productions are treated as top-level alternation
(defmacro compile-regex (name defn)
  (print "Regex " name " = ")
  (writeln defn)
  (let ((state (new-state)))
       (add-edge 0 state :eps)
       (setq state (compile state defn))
       (add-edge state name :final) ))

;;; Create a macro for use with compile-regex
(defmacro define-regex (name defn)
  (print "Macro " name " = ")
  (writeln defn)
  (push (cons name defn) *macros*) )

;;; All of the following compile* functions accept a starting state and return a
;;; final state.  The definition provides the acceptable path(s) between these states
(defun compile (state defn)
  (cond
    ;; Compile a character
    ((charp defn)
     (compile-char state defn) )

    ;; Compile a string
    ((stringp defn)
     (compile-string state defn) )

    ;; Compile a macro reference
    ((symbolp defn)
     (let ((mdef (assoc defn *macros*)))
          (if mdef
              (compile state (cdr mdef))
              (error 1 defn "Macro not defined") )))

    ;; No other atoms are meaningful
    ((atom defn)
     (error 1 defn "Can't compile this") )

    ;; Compile a Kleene star
    ((eq '* (car defn))
     (compile-star state (cdr defn)) )

    ;; Compile alternative
    ((eq '| (car defn))
     (compile-alternative state (cdr defn)) )

    ;; Compile a one-or-more operator
    ((eq '+ (car defn))
     (compile-star1 state (cdr defn)) )

    ;; Compile the optional operator
    ((eq '? (car defn))
     (compile-optional state (cdr defn)) )

    ;; Compile the range operator
    ((eq '- (car defn))
     (compile-range state (cdr defn)) )

    ;; Compile a succession
    (t
     (compile-list state defn) )))

;; Compile a character
(defun compile-char (state c)
  (add-edge state (new-state) c) )

;; Compile a string
(defun compile-string (state s)
  (let ((c (chars s)))
       (loop (while c state)
             (setq state (add-edge state (new-state) (car c))
                   c     (cdr c) ))))

;; Compile a Kleene star
(defun compile-star (state defn)
  (slet ((lp (new-state))
         (out (compile lp defn)) )
        (add-edge out lp :eps)
        (add-edge state out :eps) ))

;; Compile alternative
(defun compile-alternative (state alt)
  (let ((next (new-state)))
       (loop
         (while alt next)
         (add-edge (compile state (car alt)) next :eps)
         (pop alt) )))

;; Compile a one-or-more operator
(defun compile-star1 (state defn)
  (let ((lp (compile state defn)))
       (add-edge lp state :eps)
       (add-edge lp (new-state) :eps) ))

;; Compile the optional operator
(defun compile-optional (state defn)
  (let ((next (new-state)))
       (add-edge state next :eps)
       (add-edge (compile state defn) next :eps) ))

;; Compile the range operator
(defun compile-range (state defn)
  (let ((next (new-state))
        (s (integer (car defn)))
        (e (integer (cadr defn))) )
       (if (> s e) (psetq s e e s))
       (for (c s e)
           (add-edge state next (char c)) )
       next ))

;; Compile a succession
(defun compile-list (state defn)
  (if (null (cdr defn))
      (compile state (car defn))  ; One item
      (loop                       ; More than one
        (while defn state)
        (setq state (compile state (car defn)))
        (pop defn) )))

;;; Dole out new states on demand
(defun new-state nil
  (if (<= *next-nfa-state* MAX_STATES)
      (prog1 *next-nfa-state* (inc *next-nfa-state*))
      (error 1 *next-nfa-state* "Too many states") ))

;;; Add an edge to the NFA
(defun add-edge (from to ch)
  (vset *nfa-states* from (cons (cons ch to) (vref *nfa-states* from)))
  to )

;;; Determine the closure of a set of states in the NFA
(defun closure (states)
  (let ((edges) (cl states) )
       (loop
         (while states cl)
         (setq edges (vref *nfa-states* (car states)))
         (loop (while edges cl)
               (when (and (eq (caar edges) :eps)
                          (not (member (cdar edges) cl)) )
                 (setq cl (merge (closure (list (cdar edges)))
                                 cl )))
               (pop edges) )
         (pop states) )))

;;; Find the destination of an edge of the dfa out of states via character c,
;;; where states is the set of NFA states represented by the present DFA state.
;;; For every previously unvisited NFA state that is found add its closure to
;;; the destination dfa state.
(defun dfa-edge (states c)
  (let ((cl) (edges))
       (setq states (closure states))
       (loop
         (while states cl)
         (setq edges (vref *nfa-states* (car states)))
         (loop (while edges cl)

               (if (and (not (member (cdar edges) cl))
                        (eql (caar edges) c) )
                   (setq cl (merge (closure (list (cdar edges)))
                                   cl)) )
               (pop edges) )
         (pop states) )))

;;; Convert an NFA to a DFA
;;; Iteratively visit all reachable DFA states by testing all transitions
;;; out of the states accumulated so far.
(defun nfa-dfa nil
  (let ((p 1) (j 0) (states) (states-left) (edges) (ch) (e) (i))
       (vset *dfa-states* 1 (closure '(0)))
       ;; Iterate over DFA states
       (loop
         (while (<= j p))
         (setq states (vref *dfa-states* j)
               states-left states
               ch nil )
         ;; Iterate over the NFA states...
         (loop
           (while states-left)
           (setq edges (vref *nfa-states* (car states-left)))
           ;; ...and their edges
           (loop
             (while edges)
             (setq c (caar edges))
             ;; First time this character tried?
             (when (and (charp c) (not (member c ch)))
               (push c ch)
               (setq e (dfa-edge states c))
               (setq i 0)
               ;; Loop over existing states
               (loop
                 (while (<= i p)
                        (inc p)
                        (if (>= p MAX_STATES) (error 1 :none "Out of DFA states"))
                        (vset *dfa-states* p e)
                        (vset *dfa-trans* j (cons (cons c p) (vref *dfa-trans* j))) )
                 (until (equal e (vref *dfa-states* i))
                        (vset *dfa-trans* j (cons (cons c i) (vref *dfa-trans* j))) )
                 (inc i) ))
             ;; Just copy final state flags over
             (when (eq c :final)
                   (vset *dfa-trans* j (append (vref *dfa-trans* j) (list (car edges)))) )
             (pop edges) )
           (pop states-left) )
         (inc j) )))

;;; Merge (non-unique) elements of x into sorted set of unique elements, y
;;; Sorting makes things easier to read and simplifies the algorithm
(defun merge (x y)
  (loop
    (while x y)
    (unless (member (car x) y)
      (setq y (insert (car x) y)) )
    (setq x (cdr x)) ))

; Insert a number into a sorted list
(defun insert (n l)
  (cond
    ((not l)
     (list n) )
    ((< n (car l))
     (cons n l) )
    (t
     (cons (car l)(insert n (cdr l))) )))

;;; Free up globals no longer required for interpretation
;;; of the DFA.  Only *dfa-trans* is needed
(defun cleanup nil
  (destroy '*nfa-states* '*dfa-states*) )

;;; Run the constructed DFA on a string
;;; Return a successful result as a list of dotted pairs
;;; each having a terminal name as car and the corresponding
;;; text as cdr.
;;; In the event of a parse error then the list will
;;; contain a list of the :fail key, the dfa state
;;; and the character which failed to parse.
;;; Parse errors will always appear as the last entry in the
;;; list.
(defun run-dfa (str)
  (let ((ch (chars str))
        (state 1)
        (edge)
        (final)
        (token)
        (result) )
       (loop
         ;; Anything more to parse?
         (while ch
           (when (<> state 1)
             (setq final (dfa-next :final state))
             (push
               (if final
                 ;; Valid final state
                 (cons (cdr final) (string (reverse token)))
                 ;; Invalid final state - note the error
                 (break (list :fail state (failat ch))) )
               result ) ))

         ;; Get the edge to the next state
         (setq edge (dfa-next (car ch) state))
         (if edge
             ;; Got an edge, follow it
             (progn
               (setq state (cdr edge))
               (push (pop ch) token) )
             ;; No valid edge found. Final state?
             (progn
               (setq final (dfa-next :final state))
               (push
                 (if final
                   ;; Valid final state
                   (cons (cdr final) (string (reverse token)))
                   ;; Invalid final state - note the error
                   (break (list :fail state (failat ch))) )
                 result )
                (setq token nil
                      state 1 ))))
       ;; Reverse the pushed results
       (reverse result) ))

;;; Get an edge from state, matching character trans from the dfa.  Will
;;; also find :final pseudo-edges if :final is the first argument.
;;; There may be more than one of these and the first will be returned.
;;; Return nil on failure
(defun dfa-next (trans state)
  (assoc trans (vref *dfa-trans* state)) )

;; Return the failed character or :eol at end of line
(defun failat (x)
  (if (null x)
      :eol
      (car x) ))

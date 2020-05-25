;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Extract Lisp function help from func.cpp
;;;; Generates the file LispRef.html in a form suitable for conversion
;;;; to an MS-Reader document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Open files as globals infile1/2 and outfile
(defun open-files nil
  (setq infile1 (open-file "func.cpp" :read))
  (when (null infile)
    (error 1 "func.cpp" "Can't open input file") )

  (setq infile2 (open-file "gfunc.cpp" :read))
  (when (null infile)
    (error 1 "gfunc.cpp" "Can't open input file") )

  (setq outfile (open-file "LispRef.html" :create))
  (when (null outfile)
    (error 1 "LispRef.rtf" "Can't open output file") ))

;;; ...and cleanup
(defun close-files nil
  (close-file infile1)
  (close-file infile2)
  (close-file outfile) )

;;; Define some flags
(defkey :func)
(defkey :macr)
(defkey :desc)
(defkey :exam)

;; Special character association list and search string
(defconstant :spconv '((#\&  "amp;")(#\< "lt;")(#\> "gt;")))
(defconstant :spchar "&<>")

;;; Read the next line of help
;;; Returns a list of :func, :macr, :desc or :exam followed by the trimmed line
;;; Returns nil at EOF
(defun get-help-line nil
  (let ((line)(hdr))
       (loop (while (not (eof infile)) nil)
             (setq line (readline infile))
             (setq hdr (substring line 0 4))
             (until (eql hdr "//f ")
                    (list :func (escape (substring line 3))) )
             (until (eql hdr "//m ")
                    (list :macr (escape (substring line 3))) )
             (until (eql hdr "//$ ")
                    (list :desc (escape (substring line 4))) )
             (until (eql hdr "//x ")
                    (list :exam (escape (substring line 4))) )))) 

;;; Convert any special characters
(defun escape (s)
   (if (not (string-search s :spchar))
       s
       ;; Add and remove a space to ensure that any changes are
       ;; after the first character
       (string (cdr (do-escape (cons :space (chars s))))) ))

;;; Destructively insert codes into a list of characters
(defun do-escape (c)
  (let ((ci c)(temp)(spch)(next))
       (loop
         (while ci c)
         (if (setq spch (assoc (car ci) :spconv))
           (progn
             (setq temp (append (chars (cadr spch)) (cdr ci)))
             (setq next (cdr ci)) ; Pop the special char and note where we are...
             (rplacd ci temp)
             (rplaca ci #\&)
             (setq ci next) )     ; ...and continue from there
            (pop ci) ))))

;;; Scan for help lines and collect them for formatting
(defun scan-help nil
  (let ((func) (desc) (exam) (line) (count 0))
       (loop
          (setq line (get-help-line))
          (until (null line)
                 (print-help func desc exam)
                 count )
          (cond
            ((member (car line)                ; Got a new header?
                     '(:func :macr) )
             (when func
               (print-help func desc exam) )   ; Print the last one

             ;; Setup for the next entry and set its type
             (setq func
                  (concat (if (eq (car line) :func)
                              "Function "
                              "Macro    " )
                          (cadr line) ))
             (setq desc nil)
             (setq exam nil) )

            ((eq (car line) :desc)             ; Collect description lines
             (push (cadr line) desc) )

            ((eq (car line) :exam)             ; Collect example lines
             (push (cadr line) exam) )))))

;;; Print help for one function
(defun print-help (func desc exam)
  (inc count)
  (print-func func)
  (print-desc (reverse desc))                  ; These have been pushed in reverse order
  (print-exam (reverse exam)) )                ; ...as have these
 

;;; Print the parts of the help output
(defun print-func (func)
  (println outfile "<h4>")
  (println outfile func)
  (println outfile "</h4>") )

(defun print-desc (desc)
  (println outfile "<p>")
  (loop (while desc)
        (println outfile (pop desc) " ") ))

(defun print-exam (exam)
  (when exam
    (println outfile "<br /><b>Examples:</b> <br />")
    (println outfile "<div class=\"ex\">")
    (loop (while exam)
          (println outfile (pop exam) "<br />") )
    (println outfile "</div>") )
  (println outfile "</p>") )

(defun print-html-header nil
  ;; Not a good idea without broadband net access!
  ;; (println outfile "<!DOCTYPE foo SYSTEM \"http://msdn.microsoft.com/xml/general/htmlentities.dtd\">")
  (println outfile "<html>")
  (println outfile "<head>")
  (println outfile "<title>AJBLisp Function Reference</title>")
  ;; CSS specification
  (println outfile "<style>")
  (println outfile "h4")
  (println outfile "{")
  (println outfile "  page-break-before: always;")
  (println outfile "  font-size: small;")
  (println outfile "  font-weight: bold")
  (println outfile "}")
  (println outfile ".ex")
  (println outfile "{")
  (println outfile "  font-size: x-small;")
  (println outfile "  text-indent: -1em;")
  (println outfile "  text-align: left")
  (println outfile "}")
  (println outfile "p")
  (println outfile "{")
  (println outfile "  text-align: left")
  (println outfile "}")
  (println outfile "</style>")
  (println outfile "</head>")
  (println outfile "<body>")
  (println outfile "<h1>AJBLisp Function Reference</h1>")
  (println outfile "<br />")
  (println outfile "<br />") )

(defun print-html-footer nil
  (println outfile "</body>")
  (println outfile "</html>") )

;;; Put it all together...
(defun extract-help nil
  (guard
    (open-files)
    (setq infile infile1)   ; Scan func.cpp
    (print-html-header)
    (setq funcount (scan-help))
    (setq infile infile2)   ; Scan gfunc.cpp
    (inc funcount (scan-help))
    (println funcount " functions processed.")
    (print-html-footer)
    (close-files) ))

;;; ...and from the top...
(extract-help)

;; Close if launched from an icon
(when (scripted)
  (sleep 5000)
  (exit) )

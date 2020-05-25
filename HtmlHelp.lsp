;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Extract Lisp function help from func.cpp
;;;; Generates the file AJBLisp-Help.html and the directory Help containing
;;;; the individual help pages and stylesheet.
;;;;
;;;; Run from the target directory (which may be given on the command line)
;;;; Expects source files to be in target\src\
;;;; e.g. AJBLisp -lHtmlHelp c:\AJBLisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(backtrace 5)

;;; Set target directory as current
(when (consp cmdline)
      (cd (car cmdline)) )

(setq doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\r\n")

;;; Open files as globals infile1/2 and outfile
(defun open-files nil
  (setq infile1 (open-file "src\\func.cpp" :read))
  (when (null infile)
    (error 1 "src\\func.cpp" "Can't open input file") )

  (setq infile2 (open-file "src\\gfunc.cpp" :read))
  (when (null infile)
    (error 1 "src\\gfunc.cpp" "Can't open input file") )

  (setq errfile (open-file "src\\errors.lsp" :read))
  (when (null errfile)
    (error 1 "src\\errors.lsp" "Can't open input file") )

  (setq indexfile (open-file "AJBLisp-Help.htm" :create))
  (when (null indexfile)
    (error 1 "AJBLisp-Help.htm" "Can't open index file") ))

;;; ...and cleanup
(defun close-files nil
  (close-file infile1)
  (close-file infile2)
  (close-file errfile)
  (close-file indexfile) )

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
       ;; after the first character in the list
       (string (cdr (do-escape (cons :space (chars s))))) ))

;;; Destructively insert codes into a list of characters
(defun do-escape (c)
  (let ((ci c)(temp)(spch)(next))
       (loop
         (while ci c)
         (setq next (cdr ci)) ; Pop the special char and note where we are...
         (when (setq spch (assoc (car ci) :spconv))
               (setq temp next)
               (for-each (ct (reverse (chars (cadr spch))))
                        (setq temp (cons ct temp)) )
               (rplacd ci temp)
               (rplaca ci #\&) )
         (setq ci next) )))

;;; Scan for help lines and collect them for formatting
(defun scan-help ((count . 0))
  (let ((func) (desc) (exam) (line) (fm))
       (loop
          (setq line (get-help-line))
          (until (null line)
                 (print-help fm func desc exam (inc count))
                 count )
          (cond
            ((member (car line)                ; Got a new header?
                     '(:func :macr) )
             (when func
               (print-help fm func desc exam (inc count)) )   ; Print the last one

             ;; Setup for the next entry and set its type
             (setq fm
                  (if (eq (car line) :func)
                          "Function "
                          "Macro    " ))
             (setq func (cadr line))
             (setq desc nil)
             (setq exam nil) )

            ((eq (car line) :desc)             ; Collect description lines
             (push (cadr line) desc) )

            ((eq (car line) :exam)             ; Collect example lines
             (push (cadr line) exam) )))))

;;; Print help for one function
(defun print-help (fm func desc exam count)
  (inc count)
  (setq fname (concat "Help\\" (string count) ".htm"))
  (setq outfile (open-file fname :create))
  (when (null outfile)
    (error 1 fname "Can't open output file") )

  (guard
    (print-html-header fm func (toc-entry func count))
    (print-desc (reverse desc))                  ; These have been pushed in reverse order
    (print-exam (reverse exam))                  ; ...as have these
    (print-html-footer)
    ; guarded
    (close-file outfile) ))


(defun print-desc (desc)
 (println outfile "<p>")              ;; TEST
 (let ((line)(pre))
       (loop (while desc)
             (setq line (pop desc))
             (when (eql #\~ (char line 0))
                   (println outfile "</p><p>")
                   (setq line (substring line 1)) )
             (if (eql #\. (char line 0))
               (progn
                 (when (not pre)
                    (println outfile "<pre>")
                    (setq pre t) )
                 (println outfile (substring line 1)) )
               (progn
                 (when pre
                    (println outfile "</pre>")
                    (setq pre nil) )
                 (println outfile line) )))
       (when pre
          (println outfile "</pre>") ))
  (println outfile "<p>") )            ;; TEST


(defun print-exam (exam)
  (when exam
    (println outfile "<br /><br /><b>Examples:</b>")
    (print outfile "<pre>")
    (loop (while exam)
          (when (string= "&gt;" (substring (car exam) 0 4))
                (println outfile) )
          (println outfile (pop exam)) )
    (println outfile "</pre>") ))

(defun print-html-header (fm func shortfunc)
  (println outfile doctype)
  (println outfile "<html>")
  (println outfile "<head>")
  (println outfile "<title>" shortfunc "</title>")
  ;; CSS specification
  (println outfile "<link rel=\"stylesheet\" type=\"text/css\" href=\"help.css\">")
  (println outfile "</head>")
  (println outfile "<body>")
  (println outfile "<h1><br />" func "</h1>")
  (println outfile "<b>" fm "</b><br />") )

(defun print-html-footer nil
  (println outfile "</body>")
  (println outfile "</html>") )

(defun read-errors nil
  (let ((errno 0))
    (setq *errors* (vector (read errfile)))
    (for-each (line *errors* index)
      (when (consp line)
        (cond
          ((eq '+ (car line))
           (vset *errors* index (cons (inc errno) (cdr line))) )
          ((eq '- (car line))
           (vset *errors* index (cons (dec errno) (cdr line))) )
          (t
           (setq errno (car line)) ))))))

(defun error-help nil
  (let
    ((f (open-file "Help\\errors.htm" :create)) (incomment nil))
    (when (null f)
      (error 1 fname "Can't open error reference file") )
    (guard
      (println f doctype)
      (println f "<html>")
      (println f "<head>")
      (println f "<title> Error codes </title>")
      ;; CSS specification
      (println f "<link rel=\"stylesheet\" type=\"text/css\" href=\"help.css\">")
      (println f "</head>")
      (println f "<body>")
      (println f "<h1><br /> Error Codes </h1>")
      (println f "<table>")

      (for-each (line *errors*)
        (println f "<tr> ")
        (if (stringp line)
            (progn
              (if incomment
                (println f "<td colspan=3><i>" line "</i></td>")
                (println f "<td colspan=3 bgcolor=\"#D0D0D0\">" line "</td>") )
              (setq incomment t) )
            (progn
              (println f "<td align=\"right\">" (nth 0 line) "</td>"
                         "<td> " (nth 1 line) "</td>"
                         "<td> " (nth 2 line) "</td>" )
              (setq incomment nil) ))
        (println f "</tr> ") )

      (println f "</table")
      (print-html-footer)
      (close-file f) )
      (println "Help\\errors.htm created") ))


(defun toc-entry (func file)
  ; Print a dot for each entry
  (print #\.)
  (flush :stdout)
  (setq func (substring func 2))
  (setq func (substring func 0 (string-search func " ()")))
  (push (cons func file) *toc*)
  func)

(defun write-contents ()
  (println indexfile doctype)
  (println indexfile "<html>")
  (println indexfile "<head>")
  (println indexfile "<title> AJBLisp Function Index </title>")
  ;; CSS specification
  (println indexfile "<link rel=\"stylesheet\" type=\"text/css\" href=\"Help/help.css\">")
  (println indexfile "</head>")
  (println indexfile "<body>")
  (println indexfile "<h1><br/>AJBLisp Function Index</h1>")
  (msort *toc* toc>)
  (setq sect nil)

  ; Link to error reference
  (println indexfile "<a href=\"Help/errors.htm\">Error Reference</a><hr />")

  ; Output links to each function's page
  (for-each (f *toc*)
    ; Print the section separator, if there is a new initial letter
    (do-section (char (car f) 0))
    (print-format indexfile "<a href=\"Help/%s.htm\">%a</a> &nbsp;&nbsp;\n" (string (cdr f)) (car f)) )


  (println indexfile "<br />")
  (print-html-footer) )

(defun cleanup nil
  ; Does Help exist?
  (let ((help (dir "Help")))
    (cond
      ; If not then create it
      ((null help)
       (create-directory "Help") )

      ; It's a file. Nuke it.
      ((stringp (car help))
       (delete-file (car help))
       (create-directory "Help") )

      ; It's a directory. Clear it.
      ((consp (car help))
       ; css file is copied read-only
       (protect-file "Help\\help.css" nil)
       (for-each (f (dir "Help\\*.*"))
         (when (stringp f)
          (delete-file (concat "Help\\" f)) )))))

  ; Install the style sheet
  (copy-file "src\\help.css" "Help\\help.css") )

(defun toc> (tocx tocy) (string> (car tocx) (car tocy)))

(setq section #\`)

(defun do-section (s)
  (when (and (char<> s #\&) (char> s section))
        (setq section s)
        (println indexfile "<br><br>" (upper-case section) "<hr>") ))

;;; Put it all together...
(defun extract-help nil
  (cleanup)
  (guard
    (open-files)
    (setq *toc* nil)
    (setq infile infile1)   ; Scan func.cpp
    (setq funcount (scan-help))
    (setq infile infile2)   ; Scan gfunc.cpp
    (setq funcount (scan-help funcount))
    (write-contents)
    (destroy '*toc*)
    (println)
    (println funcount " functions processed.")
    (read-errors)
    (error-help)
    (destroy '*errors*)
    ; guarded
    (close-files)))

;;; ...and from the top...
(extract-help)

;; Close if launched from an icon or command line
(when (scripted)
      (println)
      (println "Closing....")
      (sleep 2000)
      (exit) )

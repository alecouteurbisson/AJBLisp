(defun read-errors (fname)
  (let
    ((f (open-file fname :read)) (errno))
    (guard
      (setq *errors* (read f))
      (close-file f) )
    (for-each (line *errors* index)
      (when (consp line)
        (cond
          ((eq '+ (car line))
           (vset *errors* index (cons (inc errno) (cdr line))) )
          ((eq '- (car line))
           (vset *errors* index (cons (dec errno) (cdr line))) )
          (t
           (setq errno (car line)) ))))))

(defun warning (f)
  (println f "// Generated file - DO NOT EDIT")
  (println f "// Edit errors.lsp to make changes") )


(defun lisperr.h nil
  (let
    ((f (open-file "lisperr.h" :create)))
    (guard
      (warning f)
      (println f   "#ifndef LisperrH")
      (println f   "#define LisperrH")
      (println f)
      (println f   "enum LERROR {")
      (for-each (line *errors*)
        (if (stringp line)
            (println f "// " line)
            (print-format f
                          "   %-17s=%4d,\n"
                          (upper-case (substring (string (cadr line)) 1))
                          (car line) )))

      (println f   "   LERR_DUMMY };")
      (println f)
      (println f   "const char* err_msg(LERROR e);")
      (println f   "void DefineErrorConst();")
      (println f)
      (println f   "#endif") )
      (close-file f) )
      (println "lisperr.h created") )

(defun lisperr.cpp nil
  (let
    ((f (open-file "lisperr.cpp" :create)))
    (guard
      (warning f)
      (println f   "#include \"ajblisp.h\"")
      (println f)
      (println f   "const char* err_msg(LERROR e)")
      (println f   "{")
      (println f   "  char* msg;")
      (println f   "  switch(e)")
      (println f   "  {")

      (for-each (line *errors*)
        (if (stringp line)
            (println f "// " line)
            (print-format f
                          "    case  %-17s: msg = %w; break;\n"
                          (upper-case (substring (string (cadr line)) 1))
                          (nth 2 line) )))

      (println f   "    default                : msg = \"Unknown error\";")
      (println f   "  }")
      (println f   "  return msg;")
      (println f   "}")
      (println f)
      (println f)
      (println f   "void DefineErrorConst()\n{")

      (for-each (line *errors*)
        (if (stringp line)
            (println f "// " line)
            (print-format f
                          "  NewConstant(%18w, NewInteger(%d));\n"
                          (string (cadr line))
                          (car line) )))

      (println f   "}")
      (close-file f) )
      (println "lisperr.cpp created") ))


(read-errors "errors.lsp")
(lisperr.h)
(lisperr.cpp)

(when (scripted)
  (sleep 1000)
  (exit))

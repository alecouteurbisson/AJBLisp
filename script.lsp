(defun do-scripted ()
  (println "I an running in a script")
  (sleep 5000)
  (exit) )

(defun do-interactive ()
  (println "I am running interactively") )

(if (scripted)
    (do-scripted)
    (do-interactive))

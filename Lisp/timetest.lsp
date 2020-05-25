(defun tt((tm))
  (if (not tm) (setq tm (now)))

  (println "Setting TZ to EST5EDT")
  (tzset "EST5EDT")
  (local-time nil)
  (println "(local-time nil) ->" (time-string "%#c %Z" tm))
  (local-time t)
  (println "(local-time t)   ->" (time-string "%#c %Z" tm))
  (println)

  (println "Setting TZ to GMT0BST")
  (tzset "GMT0BST")
  (local-time nil)
  (println "(local-time nil) ->" (time-string "%#c %Z" tm))
  (local-time t)
  (println "(local-time t)   ->" (time-string "%#c %Z" tm))
  (println)

  (println "Setting dst off")
  (dst nil)
  (local-time nil)
  (println "(local-time nil) ->" (time-string "%#c %Z" tm))
  (local-time t)
  (println "(local-time t)   ->" (time-string "%#c %Z" tm))

  nil )


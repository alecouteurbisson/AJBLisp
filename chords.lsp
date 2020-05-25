(defun read-chords (cfname)
  (let
    ((f)(chord)(chords))
    (setq f (open cfname :read))
    (loop
      (setq chord (read f))
      (until (eq chord 'end) (close f) (reverse chords))
      (push (list (string (cadr chord))
                  (string (car chord))
                  (nthcdr 2 chord) )
            chords ))))

(setq CHORDS (read-chords "chords.txt"))


(setq *running-status* nil)

(defun midi-dump (mf)
  (let ((f))
    (guard
       (setq f (open-file mf :read))
       (dump-all f)
       ;; Always executed
       (close-file f) )))

(defun dump-all (f)
  (let ((len)(fmt)(ntracks)(delta-t))
    (setq len (check-header f "MThd"))
    (println "Header Chunk (length " len ")")
    (if (<> len 6)
        (error 1 len "Bad header length") )
    (setq fmt     (read-midi-word f))
    (setq ntracks (read-midi-word f))
    (setq delta-t (read-midi-word f))
    (println "File format     : " fmt)
    (println "Number of tracks: " ntracks)
    (println "Delta time      : " delta-t)
    (for (k 1 ntracks) (dump-track k)) ))

(defun dump-track (trk)
  (let ((len))
    (setq len (check-header f "MTrk"))
    (println "Track Chunk (length " len ")")
    (file-position f len :seek-relative) ))

(defun dump-event (f)
  (let ((delta)(byte)(event))
    (setq delta (read-midi-var))
    (setq byte (readbyte f))
    (cond
      ((statusp byte)
        (setq event byte
              *running-status* byte) )
      (t
       (setq event *running-status*)
       (unread byte) ))

    ;; Now decode the event
    
    ))

(defun decode-meta-event (et)
  (case et
    (0x00   "Sequence number")
    (0x01   "Text")
    (0x02   "Copyright")
    (0x03   "Track name")
    (0x04   "Instrument")
    (0x05   "Lyric")
    (0x06   "Marker")
    (0x07   "Cue point")
    (0x20   "Midi channel")
    (0x21   "Midi port")
    (0x2F   "End of track")
    (0x51   "Tempo")
    (0x54   "SMPTE offset")
    (0x58   "Time signature")
    (0x59   "Key signature")
    (0x7F   "Proprietary event")
    (:else  (error 1 et "Unknown meta-event type")) ))


;; Check for a specific chunk header
(defun check-header (f hdr)
  (let ((len))
    (for (k 0 3)
      (if (not (eql (char hdr k) (readchar f)))
          (error 1 hdr "Bad header") ))
    (read-midi-long f) ))

;; Read a long-word in big-endian format
(defun read-midi-long (f)
  (let ((long 0))
    (for (k 0 3)
      (setq long (bor (bshift long 8)
                      (readbyte f) )))
    long ))

;; Read a word in big-endian format
(defun read-midi-word (f)
  (bor (bshift (readbyte f) 8) (readbyte f)) )

;; Read a variable length value
(defun read-midi-var (f)
  (let ((byte) (val 0))
    (loop
      (setq byte (readbyte f)
            val  (bor (bshift val 7)
                      (band val 0x7F) ))
      (while (<> (band byte 0x80) 0)
             val ))))

;; True if this is a status byte
(defun statusp (b)
  (<> (band b 0x80) 0) )

;; True if b is in [hi lo]
(defun in-range (v hi lo)
  (and (>= b lo) (<= b hi)) )

(midi-dump "chill.mid")

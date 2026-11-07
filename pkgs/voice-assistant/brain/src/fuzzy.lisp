;;; Sound-alike matching for names whisper mangles ("Laslos" for
;;; "lassulus", "Yif" for "eve"): compare a crude phonetic skeleton by
;;; edit distance.

(in-package #:va)

(defn replace-all ((s string) (old string) (new string)) string
  (with-output-to-string (out)
    (loop with start = 0
          for at = (search old s :start2 start)
          do (write-string s out :start start :end (or at (length s)))
          (cond (at (write-string new out) (setf start (+ at (length old))))
                (t (return))))))

(defn skeleton ((s string)) string
  "Lowercase letters and digits, every vowel the same, voiced and voiceless
consonants merged (they sound alike through a headset), hissing sounds
merged, no doubled letters. \"joke\" comes out as \"chalk\" or \"shrog\"."
  (let ((s (string-downcase s)))
    (loop for (old new) in '(("sch" "s") ("sh" "s") ("ch" "s") ("j" "s"))
          do (setf s (replace-all s old new)))
    (let ((out (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
      (loop for c across s
            when (and (alphanumericp c) (< (char-code c) 128))
            do (let ((c (case c
                          ((#\a #\e #\i #\o #\u #\y) #\a)
                          (#\f #\v) (#\p #\b) (#\t #\d) ((#\k #\c #\q) #\g) (#\s #\z)
                          (otherwise c))))
                 (unless (and (plusp (length out)) (char= c (char out (1- (length out)))))
                   (vector-push-extend c out))))
      (coerce out 'simple-string))))

(defn edit-distance ((a string) (b string)) (integer 0)
  (let ((prev (loop for j to (length b) collect j)))
    (loop for ca across a
          for i from 1
          do (let ((cur (list i)))
               (loop for cb across b
                     for (p0 p1) on prev
                     do (push (min (+ p0 (if (char= ca cb) 0 1)) (1+ p1) (1+ (first cur))) cur))
               (setf prev (nreverse cur))))
    (car (last prev))))

(defn closest ((heard words) (candidates list)) (or null (integer 0))
  "The id of the (id . words) candidate closest to HEARD, if close enough,
about one edit per four sounds, and unambiguous."
  (let ((h (skeleton (format nil "~{~a~}" heard)))
        best best-id tie)
    (when (< (length h) 2) (return-from closest nil))
    (loop for (id . ws) in candidates
          for c = (skeleton (format nil "~{~a~}" ws))
          for d = (edit-distance h c)
          when (<= d (ceiling (length c) 4))
          do (cond ((and best (= d best) (/= id best-id)) (setf tie t))
                   ((and best (>= d best)))
                   (t (setf best d best-id id tie nil))))
    (and (not tie) best-id)))

(in-package #:va)

;;; Spoken durations: "5 minutes", "an hour and a half", "1:30", "two minutes thirty".

(defn unit-seconds ((w string)) (or null double-float)
  (word-case w
    (("hour" "hours" "hr" "hrs" "h") 3600d0)
    (("minute" "minutes" "min" "mins" "m") 60d0)
    (("second" "seconds" "sec" "secs" "s") 1d0)))

(defn colon ((w string)) (or null double-float)
  "\"1:30\" (m:ss) or \"1:30:00\" (h:mm:ss)."
  (let ((parts (mapcar #'digits (uiop:split-string w :separator ":"))))
    (and (<= 2 (length parts) 3)
         (every #'identity parts)
         (every (lambda (n) (< n 60)) (rest parts))
         (coerce (reduce (lambda (a n) (+ (* a 60) n)) parts) 'double-float))))

(defn fraction ((w string)) (or null double-float)
  (word-case w
    ("half" 0.5d0)
    (("quarter" "quarters") 0.25d0)))

(defn duration-word-p ((w string)) boolean
  (and (or (decimal w) (number w) (unit-seconds w) (fraction w) (colon w)
           (in w "a" "an" "and" "of" "hundred" "couple" "few"))
       t))

(defn duration ((ws words)) (or null (integer 1 86400))
  "Seconds in a spoken duration. A bare number counts as minutes, a trailing
number as the next smaller unit (\"1 hour 20\")."
  (let ((total 0d0) (num nil) (fraction-only nil) (last-unit nil))
    (loop for (tok next) on ws
          for unit = (unit-seconds tok)
          for value = (or (decimal tok) (number tok))
          do (cond
               (unit
                ;; Single letters are only units right after a number ("5 m").
                (when (and (= (length tok) 1) (null num))
                  (return-from duration nil))
                (incf total (* (or num 1d0) unit))
                (setf last-unit unit num nil fraction-only nil))
               ((colon tok) (incf total (colon tok)))
               (value (setf num (+ (or num 0d0) value)))
               ((string= tok "hundred") (setf num (* (or num 1d0) 100)))
               ((fraction tok)
                ;; "three quarters" multiplies, "one and a half" adds.
                (let ((f (fraction tok)))
                  (setf num (cond ((and num (string= tok "quarters")) (* num f))
                                  (num (+ num f))
                                  (t f))
                        fraction-only (or fraction-only (and last-unit t) (= num f)))))
               ((string= tok "couple") (setf num 2d0))
               ((string= tok "few") (setf num 3d0))
               ((and (in tok "a" "an") (null num) next (unit-seconds next))
                (setf num 1d0))))
    (when num
      (incf total (* num (cond ((null last-unit) 60)
                               (fraction-only last-unit)
                               ((>= last-unit 3600) 60)
                               (t 1)))))
    ;; Half away from zero, as Rust's round.
    (let ((secs (floor (+ total 0.5d0))))
      (and (<= 1 secs 86400) secs))))

(defn duration-slot ((ws words) (next (or null string))) (or null (integer 1 86400))
  "Every word must belong to the duration and the slot must not stop short of
one (\"30\" in \"30 hours\"), so it cannot swallow or split neighbouring words."
  (and (every #'duration-word-p ws)
       (not (and next (duration-word-p next) (not (in next "a" "and"))))
       (duration ws)))

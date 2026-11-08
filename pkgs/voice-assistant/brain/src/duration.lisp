;;; Spoken durations: "5 minutes", "an hour and a half", "1:30", "two minutes thirty".

(in-package #:va)

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

(defn add-fraction ((num (or null double-float)) (tok string)) double-float
  "\"three quarters\" multiplies, \"one and a half\" adds."
  (let ((f (fraction tok)))
    (cond ((and num (string= tok "quarters")) (* num f))
          (num (+ num f))
          (t f))))

(defn count-word ((tok string) (next (or null string)) (num (or null double-float)))
    (or null double-float)
  "NUM after a counting word: \"hundred\", \"a couple\", \"a few\", \"an hour\"."
  (word-case tok
    ("hundred" (* (or num 1d0) 100))
    ("couple" 2d0)
    ("few" 3d0)
    (("a" "an") (and (null num) next (unit-seconds next) 1d0))))

(defn leftover-scale ((last-unit (or null real)) (fraction-only boolean)) real
  "Seconds per unit of a trailing number: minutes alone, the next smaller unit
after one (\"1 hour 20\"), the unit itself after a fraction."
  (cond ((null last-unit) 60)
        (fraction-only last-unit)
        ((>= last-unit 3600) 60)
        (t 1)))

(defstruct (tally (:constructor tally ()))
  "A duration being added up word by word. The seconds so far, a number
waiting for its unit and the last unit seen."
  (total 0d0 :type double-float)
  (num nil :type (or null double-float))
  (last-unit nil :type (or null double-float))
  (fraction-only nil :type boolean))

(defn tally-word ((d tally) (tok string) (next (or null string))) boolean
  "Add TOK to D. False when TOK rules out a duration."
  (let ((unit (unit-seconds tok))
        (value (or (decimal tok) (number tok))))
    (with-accessors ((total tally-total) (num tally-num)) d
      (cond (unit (tally-unit d tok unit))
            ((colon tok) (incf total (colon tok)) t)
            (value (setf num (+ (or num 0d0) value)) t)
            ((fraction tok) (tally-fraction d tok))
            (t (setf num (or (count-word tok next num) num)) t)))))

(defn tally-unit ((d tally) (tok string) (unit double-float)) boolean
  "Single letters are only units right after a number (\"5 m\")."
  (unless (and (= (length tok) 1) (null (tally-num d)))
    (incf (tally-total d) (* (or (tally-num d) 1d0) unit))
    (setf (tally-last-unit d) unit (tally-num d) nil (tally-fraction-only d) nil)
    t))

(defn tally-fraction ((d tally) (tok string)) boolean
  (let ((num (add-fraction (tally-num d) tok)))
    (setf (tally-num d) num
          (tally-fraction-only d) (or (tally-fraction-only d) (and (tally-last-unit d) t)
                                      (= num (fraction tok))))
    t))

(defn duration ((ws words)) (or null (integer 1 86400))
  "Seconds in a spoken duration. A bare number counts as minutes, a trailing
number as the next smaller unit (\"1 hour 20\")."
  (let ((d (tally)))
    (loop for (tok next) on ws
          unless (tally-word d tok next) do (return-from duration nil))
    (with-accessors ((total tally-total) (num tally-num)) d
      (when num
        (incf total (* num (leftover-scale (tally-last-unit d) (tally-fraction-only d)))))
      ;; Half away from zero, as Rust's round.
      (let ((secs (floor (+ total 0.5d0))))
        (and (<= 1 secs 86400) secs)))))

(defn duration-slot ((ws words) (next (or null string))) (or null (integer 1 86400))
  "Every word must belong to the duration and the slot must not stop short of
one (\"30\" in \"30 hours\"), so it cannot swallow or split neighbouring words."
  (and (every #'duration-word-p ws)
       (not (and next (duration-word-p next) (not (in next "a" "and"))))
       (duration ws)))

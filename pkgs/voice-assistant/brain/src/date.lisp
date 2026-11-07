(in-package #:va)

;;; Spoken calendar dates: "friday", "next friday", "march 1st", "the 15th".
;;; Parsed without knowing today and resolved against it later.

(defparameter *weekdays*
  #("sunday" "monday" "tuesday" "wednesday" "thursday" "friday" "saturday")
  "Indexed by weekday, 0 is Sunday.")

(defparameter *months*
  #("january" "february" "march" "april" "may" "june" "july" "august" "september"
    "october" "november" "december")
  "Indexed by month, 0 is January.")

(defparameter *ordinals*
  #("first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth" "tenth"
    "eleventh" "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth"
    "eighteenth" "nineteenth")
  "Spoken days of the month, 0 is the first.")

(defn day-number ((y integer) (m (integer 1 12)) (d (integer 1 31))) integer
  "Days since 1970-01-01."
  (let* ((y (if (<= m 2) (1- y) y))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) d -1))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defn civil ((n integer)) (values integer (integer 1 12) (integer 1 31))
  "Year, month and day of a day number."
  (let* ((z (+ n 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- (+ (- doe (floor doe 1460)) (floor doe 36524)) (floor doe 146096)) 365))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (d (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
         (m (if (< mp 10) (+ mp 3) (- mp 9))))
    (values (+ yoe (* era 400) (if (<= m 2) 1 0)) m d)))

(defn weekday ((n integer)) (integer 0 6) "0 = Sunday." (mod (+ n 4) 7))

(defn ordinal ((w string)) (or null (integer 1 19))
  (let ((i (position w *ordinals* :test #'string=))) (and i (1+ i))))

(defn day-of-month ((ws words)) (or null (integer 1 31))
  "\"1st\", \"15\", \"first\", \"twenty first\", \"thirtieth\"."
  (let ((n (trivia:match (if (rest ws) ws (and ws (split-digits (first ws))))
                         ((list d (or "st" "nd" "rd" "th")) (digits d))
                         ((list "twenty" o) (let ((o (below (ordinal o) 10))) (and o (+ 20 o))))
                         ((list "thirty" "first") 31)
                         ((list w) (or (ordinal w) (word-case w ("twentieth" 20) ("thirtieth" 30))
                                       (number w))))))
    (and n (<= 1 n 31) n)))

(defn month ((w string)) (or null (integer 1 12))
  "\"march\", or abbreviated: \"sept\", \"dec\"."
  (let ((i (position-if (lambda (m) (or (string= m w)
                                        (and (> (length m) 4) (>= (length w) 3)
                                             (uiop:string-prefix-p w m))))
                        *months*)))
    (and i (1+ i))))

(defn weekday-name ((w string)) (or null (integer 0 6))
  (position (if (uiop:string-suffix-p w "'s") (subseq w 0 (- (length w) 2)) w)
            *weekdays* :test #'string=))

(defn year-p ((w string)) boolean
  (let ((y (and (= (length w) 4) (digits w)))) (and y (<= 1900 y 2199) t)))

(defn month-day ((m string) (d words)) (or null va.types:Date)
  (let ((m (month m)) (d (day-of-month d))) (and m d (va.types:MonthDay m d))))

(defn parse-date ((ws words)) (or null va.types:Date)
  (let* ((ws (if (in (first ws) "on" "this" "for") (rest ws) ws))
         ;; "march 4th 2019": past years in CLINC, so the year only marks a date.
         (ws (if (and (> (length ws) 2) (year-p (car (last ws)))) (butlast ws) ws)))
    (trivia:match ws
                  ;; "friday the 1st": the date decides, the weekday is decoration.
                  ((list* (trivia:guard w (weekday-name w)) (and rest (list* "the" _)))
                   (parse-date rest))
                  ((or (list w) (list w (or "'s" "s")))
                   (let ((d (weekday-name w))) (and d (va.types:Weekday d nil))))
                  ((list "next" w)
                   (let ((d (weekday-name w))) (and d (va.types:Weekday d t))))
                  ;; "march 1st", "march the first"
                  ((list* (trivia:guard m (month m)) "the" d) (month-day m d))
                  ((list* (trivia:guard m (month m)) d) (month-day m d))
                  ;; "the 1st of march", "1st of march", "the 15th"
                  ((list* "the" rest) (or (date-of rest) (of-month rest)))
                  (_ (date-of ws)))))

(defn date-of ((ws words)) (or null va.types:Date)
  "\"1st of march\"."
  (let ((of (position "of" ws :test #'string=)))
    (and of (= (length ws) (+ of 2)) (month-day (nth (1+ of) ws) (subseq ws 0 of)))))

(defn of-month ((ws words)) (or null va.types:Date)
  "\"the 15th\"."
  (let ((d (day-of-month ws))) (and d (va.types:OfMonth d))))

(defn real-day ((y integer) (m integer) (d integer)) (or null integer)
  "The day number, if the date exists (\"february 30\" does not)."
  (when (and (<= 1 m 12) (<= 1 d 31))
    (let ((n (day-number y m d)))
      (multiple-value-bind (y2 m2 d2) (civil n)
        (and (= y y2) (= m m2) (= d d2) n)))))

(defn days-ahead ((date va.types:Date) (today integer)) (or null (integer 0))
  "Days from TODAY until DATE's next occurrence, today included. Nil for dates
that don't exist."
  (multiple-value-bind (y m) (civil today)
    (flet ((first-ahead (&rest days) (find-if (lambda (n) (and n (>= n today))) days)))
      (let ((target
             (adt-case date date
               ((weekday day next)
                (let ((ahead (mod (- day (weekday today)) 7)))
                  (+ today ahead (if (and next (zerop ahead)) 7 0))))
               ((monthday mo d) (first-ahead (real-day y mo d) (real-day (1+ y) mo d)))
               ((ofmonth d) (first-ahead (real-day y m d)
                                         (real-day (+ y (floor m 12)) (1+ (mod m 12)) d))))))
        (and target (- target today))))))

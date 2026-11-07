;;; Public holidays in Bavaria: fixed dates plus the ones that move with Easter.

(in-package #:va)

(defn easter ((y integer)) integer
  "Easter Sunday's day number (anonymous Gregorian computus)."
  (let* ((a (mod y 19)) (b (floor y 100)) (c (mod y 100)) (d (floor b 4)) (e (mod b 4))
         (f (floor (+ b 8) 25)) (g (floor (+ (- b f) 1) 3))
         (h (mod (+ (* 19 a) b (- d) (- g) 15) 30)) (i (floor c 4)) (k (mod c 4))
         (l (mod (+ 32 (* 2 e) (* 2 i) (- h) (- k)) 7))
         (m (floor (+ a (* 11 h) (* 22 l)) 451))
         (n (+ h l (* -7 m) 114)))
    (day-number y (floor n 31) (1+ (mod n 31)))))

(defn holidays ((y integer)) list
  "(day . name) for each holiday of year Y, in order."
  (let ((e (easter y)))
    (sort (list (cons (day-number y 1 1) "New Year's Day") (cons (day-number y 1 6) "Epiphany")
                (cons (- e 2) "Good Friday") (cons (+ e 1) "Easter Monday")
                (cons (day-number y 5 1) "Labour Day") (cons (+ e 39) "Ascension Day")
                (cons (+ e 50) "Whit Monday") (cons (+ e 60) "Corpus Christi")
                (cons (day-number y 8 15) "Assumption Day")
                (cons (day-number y 10 3) "German Unity Day")
                (cons (day-number y 11 1) "All Saints' Day")
                (cons (day-number y 12 25) "Christmas Day")
                (cons (day-number y 12 26) "Boxing Day"))
          #'< :key #'car)))

(defn next-holiday ((today integer)) (values integer string)
  "The first holiday after TODAY: its day number and name."
  (let* ((y (civil today))
         (h (find-if (lambda (h) (> (car h) today)) (append (holidays y) (holidays (1+ y))))))
    (if h (values (car h) (cdr h)) (values today "nothing"))))

(defparameter *day-names*
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
(defparameter *month-names*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October"
    "November" "December"))

(defn spoken-day ((n integer)) string
  "\"Thursday, November 1\"."
  (multiple-value-bind (y m d) (civil n)
    (declare (ignore y))
    (format nil "~a, ~a ~d" (aref *day-names* (weekday n)) (aref *month-names* (1- m)) d)))

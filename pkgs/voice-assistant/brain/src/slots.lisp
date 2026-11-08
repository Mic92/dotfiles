;;; Slot parsers: the words filling a template's {slot} to a value.

(in-package #:va)

(defvar *slot-parsers* (make-hash-table :test #'equal)
  "Slot name to its slot-parser, filled by defslot.")

(defmacro defslot (names (words next) &body body)
  "A slot parser for each of NAMES: WORDS, and the word after them, to a value
or nil."
  `(dolist (n ',(a:ensure-list names))
     (setf (gethash n *slot-parsers*)
           (lambda (,words ,next) (declare (ignorable ,words ,next)) ,@body))))

(defvar *closed-values* (make-hash-table :test #'equal)
  "Slot name to a function listing every value it takes, as word lists.")

(defn closed-slot ((slot string) (values function)) string
  "Declare that SLOT takes only what VALUES lists, for the lexicon."
  (setf (gethash slot *closed-values*) values)
  slot)

(defn closed-values ((slot string)) list
  (let ((f (gethash slot *closed-values*)))
    (and f (funcall f))))

(defn slot-parser ((name string)) (or null function)
  (values (gethash name *slot-parsers*)))

(defslot "duration" (ws next) (duration-slot ws next))

;; Any time of day, only after words like "at" or "until".
(defslot "clock" (ws next) (parse-clock ws))

;; A time of day that cannot be a duration ("3 pm", not "3").
(defslot "time" (ws next)
  (let ((c (parse-clock ws))) (and c (va.types:clock-marked c) c)))

(defslot "percent" (ws next)
  (let ((n (trivia:match ws
             ((list n) (number n))
             ((list tens ones) (let ((tn (number tens)) (o (below (number ones) 10)))
                                 (and tn o (+ tn o)))))))
    (and n (<= n 150) n)))

;; Stops before a following number word so "1 and 10" splits.
(defslot ("number" "low" "high") (ws next)
  (and (not (and next (number next))) (number-words ws)))

(defslot "text" (ws next)
  (and ws (unwords (substitute-if "" #'comma-p ws))))

(defslot "any" (ws next) t)

(defparameter *not-people*
  '("you" "your" "yours" "i" "me" "my" "it" "this" "that" "the" "a" "he" "she" "they" "we" "us"
    "her" "him" "anyone" "anyone's" "someone" "everyone" "everybody" "nobody" "be" "turn" "get"
    "born" "birthday" "next" "now")
  "Words that are not a contact's name.")

(defn person ((ws words)) (or null string)
  "\"shannan's\", \"bob marley\": up to three words, possessive dropped."
  (let* ((last (car (last ws)))
         (ws (if (and last (uiop:string-suffix-p last "'s"))
                 (append (butlast ws) (list (subseq last 0 (- (length last) 2))))
                 ws)))
    (and (<= 1 (length ws) 3)
         (notany (lambda (w) (member w *not-people* :test #'string=)) ws)
         (every (lambda (w) (every (lambda (c) (or (alpha-char-p c) (char= c #\-))) w)) ws)
         (unwords ws))))

(defslot "person" (ws next) (person ws))

(defslot "day" (ws next)
  (trivia:match ws
    ((list "today") 0)
    ((list "tomorrow") 1)
    ((or (list "day" "after" "tomorrow") (list "the" "day" "after" "tomorrow")) 2)
    (_ (parse-date ws))))

(defn day-offset ((day (or null (integer 0) va.types:Date))) (or null (integer 0))
  "Days ahead for what the day slot read: a number already, or a date."
  (if (integerp day) day (and day (days-ahead day (context-today *context*)))))

(defn days-from-today ((ws words)) (or null (integer 0))
  "\"tomorrow\", \"on friday\", \"march 3\" as days ahead."
  (day-offset (funcall (slot-parser "day") ws nil)))

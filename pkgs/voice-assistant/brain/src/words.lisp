(in-package #:va)

;;; Turning text into words, and number words into numbers.

(deftype words () 'list)

;; Kept by `normalize` so lists survive ("milk, eggs and bread"). Only the
;; {text} slot sees it, everything else skips it.
(alexandria:define-constant +comma+ "," :test #'equal)

(defparameter *ones*
  #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
    "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
    "eighteen" "nineteen")
  "Number words indexed by value.")

(defparameter *tens*
  #("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety")
  "Tens from twenty, indexed by value / 10 - 2.")

(defn comma-p ((w t)) boolean (equal w +comma+))

(defn digits ((w string)) (or null (integer 0))
  "W as a non-negative integer when it is only digits."
  (and (plusp (length w)) (every #'digit-char-p w) (values (parse-integer w))))

(defn decimal ((w string)) (or null double-float)
  "W as a number when it looks like 42, 3.5 or -2."
  (let ((body (string-left-trim "-" w)))
    (when (and (<= (- (length w) (length body)) 1)
               (find-if #'digit-char-p body)
               (every (lambda (c) (or (digit-char-p c) (char= c #\.))) body)
               (<= (count #\. body) 1))
      (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
        (coerce (read-from-string w) 'double-float)))))

(defn number ((w string)) (or null (integer 0))
  "A single number word or digit string."
  (or (digits w)
      (position w *ones* :test #'string=)
      (let ((i (position w *tens* :test #'string=)))
        (and i (* 10 (+ i 2))))))

(defn tens-ones ((tens string) (ones string)) (or null (integer 21 99))
  "\"twenty five\"."
  (let ((tn (number tens)) (o (number ones)))
    (and tn o (>= tn 20) (zerop (mod tn 10)) (< 0 o 10) (+ tn o))))

(defn number-words ((ws words)) (or null double-float)
  "A spoken number of one or more words: \"42\", \"twenty five\", \"one hundred
and five\", \"two thousand\", \"minus three\", \"3.5\"."
  (if (in (first ws) "minus" "negative")
      (let ((n (unsigned-number-words (rest ws)))) (and n (- n)))
      (unsigned-number-words ws)))

(defn unsigned-number-words ((ws words)) (or null double-float)
  (let ((total 0d0) (current 0d0) (seen nil))
    (loop for w in ws
          for first = t then nil
          for v = (or (decimal w) (number w))
          do (cond (v (incf current v) (setf seen t))
                   ((string= w "hundred") (setf current (* (max current 1d0) 100) seen t))
                   ((string= w "thousand")
                    (incf total (* (max current 1d0) 1000))
                    (setf current 0d0 seen t))
                   ((and first (string= w "a")))
                   ((and seen (string= w "and")))
                   (t (return-from unsigned-number-words nil))))
    (and seen (+ total current))))

(defn spell-symbols ((text string)) string
  "Arithmetic symbols as words and commas as their own token. \"1,000\" and
\"3/4\" between digits stay numbers."
  (let ((s (string-downcase text)))
    (with-output-to-string (out)
      (loop for i below (length s)
            for c = (char s i)
            for between-digits = (and (plusp i) (digit-char-p (char s (1- i)))
                                      (< (1+ i) (length s)) (digit-char-p (char s (1+ i))))
            do (case c
                 (#\+ (write-string " plus " out))
                 ((#\MULTIPLICATION_SIGN #\*) (write-string " times " out))
                 (#\DIVISION_SIGN (write-string " divided by " out))
                 (#\% (write-string " percent " out))
                 (#\= (write-string " " out))
                 (#\/ (write-string (if between-digits " over " "/") out))
                 (#\, (unless between-digits (write-string " , " out)))
                 (otherwise (write-char c out)))))))

(defn dotted-time-p ((w string)) boolean
  "\"7.30\" as the speech recogniser writes 7:30. One decimal stays a number."
  (let ((dot (position #\. w)))
    (and dot (<= 1 dot 2) (= (- (length w) dot 1) 2)
         (every #'digit-char-p (remove #\. w :count 1)))))

(defn split-digits ((w string)) list
  "W cut between digits and letters: \"10min\" -> \"10\" \"min\"."
  (let ((cuts (loop for i from 1 below (length w)
                    for a = (char w (1- i)) for b = (char w i)
                    when (or (and (digit-char-p a) (alpha-char-p b))
                             (and (alpha-char-p a) (digit-char-p b)))
                    collect i)))
    (loop for (start end) on (cons 0 cuts)
          for piece = (subseq w start end)
          when (plusp (length piece)) collect piece)))

(defn split-raw ((s string)) list
  "S cut at everything but letters, digits and . ' : ,"
  (loop with start = 0
        for i from 0 to (length s)
        when (or (= i (length s))
                 (not (or (alphanumericp (char s i)) (find (char s i) ".':,"))))
        collect (subseq s start i) and do (setf start (1+ i))))

(defn normalize ((text string)) words
  "Lowercase words without punctuation. Hyphens split (\"twenty-five\"), as do
digit/letter boundaries (\"10min\", \"7pm\"). \"1:30\" and \"o'clock\" stay whole."
  (let (out)
    (dolist (raw (split-raw (spell-symbols text)))
      (let* ((w (string-trim ".':" raw))
             (w (word-case w ("a.m" "am") ("p.m" "pm") (t w)))
             (w (if (dotted-time-p w) (substitute #\: #\. w :count 1) w)))
        (dolist (piece (split-digits w))
          (unless (and (comma-p piece) (or (null out) (comma-p (car out))))
            (push piece out)))))
    (when (comma-p (car out)) (pop out))
    (nreverse out)))

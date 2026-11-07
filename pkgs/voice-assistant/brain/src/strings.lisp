;;; Joining, splitting, trimming and cutting text.

(in-package #:va)

(defn unwords ((ws list)) string
  "WS joined by spaces."
  (format nil "~{~a~^ ~}" ws))

(defn take ((n (integer 0)) (xs sequence)) sequence
  "The first N elements of XS, or all of them."
  (subseq xs 0 (min n (length xs))))

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Return)
  "What trim and split-words treat as space.")

(defn trim ((s string)) string
  (string-trim *whitespace* s))

(defn split-words ((s string)) list
  "S split at runs of whitespace."
  (remove "" (uiop:split-string s :separator *whitespace*) :test #'string=))

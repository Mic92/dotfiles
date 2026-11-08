;;; The context of a request: the time, the day and what is going on.

(in-package #:va)

(defstruct (context (:constructor make-context (now today &key previous timer music)))
  "Seconds since midnight, the day number of today, and what is going on: the
last action if recent, whether a timer runs or rang a moment ago, and whether
music played when we started listening."
  (now 0 :type (integer 0))
  (today 0 :type integer)
  (previous nil)
  (timer nil :type boolean)
  (music nil :type boolean))

(defvar *context* (make-context (* 9 3600) (day-number 2025 10 22))
  "The request being handled. Tests bind their own.")

(defn now () (integer 0) (context-now *context*))

(defn hour () (integer 0 23) (floor (mod (now) 86400) 3600))

(defn late-p () boolean
  "23:00 to 5:00."
  (or (>= (hour) 23) (< (hour) 5)))

(defn early-p () boolean
  "5:00 to 8:00."
  (<= 5 (hour) 7))

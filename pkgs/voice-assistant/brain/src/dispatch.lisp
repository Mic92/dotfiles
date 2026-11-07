;;; From words to an action: slot parsers, requests and the best match.

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

(defn current-context (&key (previous (or null action) nil) (timer boolean nil)
                            (music boolean nil))
    context
  "The context for a request now, in local time."
  (multiple-value-bind (sec min hour d m y) (get-decoded-time)
    (make-context (+ sec (* 60 min) (* 3600 hour)) (day-number y m d)
                  :previous previous :timer timer :music music)))

(defn hour () (integer 0 23) (floor (mod (now) 86400) 3600))

(defn late-p () boolean
  "23:00 to 5:00."
  (or (>= (hour) 23) (< (hour) 5)))

(defn early-p () boolean
  "5:00 to 8:00."
  (<= 5 (hour) 7))

(defstruct (action (:constructor make-action (command fields)))
  "A command and the field values to run it with."
  (command nil :type command)
  (fields nil :type list))

(defvar *slot-parsers* (make-hash-table :test #'equal)
  "Slot name to its slot-parser, filled by defslot.")

(defmacro defslot (names (words next) &body body)
  "A slot parser for each of NAMES: WORDS, and the word after them, to a value
or nil."
  `(dolist (n ',(a:ensure-list names))
     (setf (gethash n *slot-parsers*)
           (lambda (,words ,next) (declare (ignorable ,words ,next)) ,@body))))

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
(defslot "day" (ws next)
  (trivia:match ws
    ((list "today") 0)
    ((list "tomorrow") 1)
    ((or (list "day" "after" "tomorrow") (list "the" "day" "after" "tomorrow")) 2)
    (_ (parse-date ws))))

(defparameter *lead-ins*
  '(("can" "you" "please" "tell" "me") ("can" "you" "tell" "me") ("could" "you" "tell" "me")
    ("would" "you" "tell" "me") ("would" "you" "let" "me" "know") ("please" "tell" "me")
    ("tell" "me") ("i" "need" "to" "know") ("i" "want" "to" "know") ("i" "wanna" "know")
    ("i'd" "like" "to" "know") ("i" "would" "like" "to" "know") ("i" "wonder")
    ("do" "you" "know") ("let" "me" "know") ("inform" "me") ("look" "up") ("please" "find")
    ("find" "out"))
  "Ways of asking that wrap the actual question.")

(defparameter *tails*
  '(("for" "me") ("please") (",") ("," "i" "need" "to" "know") ("," "can" "you" "tell" "me"))
  "Endings that wrap the actual question, as in \", can you tell me\".")

(defn unwrap-request ((ws words)) words
  "\"can you tell me what time it is, please\" -> \"what time it is\"."
  (loop
   (let ((before (length ws)))
     (loop while (in (first ws) "," "please" "alexa") do (pop ws))
     (let ((lead (find-if (lambda (l) (a:starts-with-subseq l ws :test #'equal)) *lead-ins*)))
       (when lead (setf ws (nthcdr (length lead) ws))))
     (let ((tail (find-if (lambda (s) (a:ends-with-subseq s ws :test #'equal)) *tails*)))
       (when tail (setf ws (butlast ws (length tail)))))
     (when (= (length ws) before) (return ws)))))

(defn resolve-dates ((slots slots)) (values slots boolean)
  "Calendar dates as days from today, the only form skills see. False when
a date doesn't exist."
  (let ((ok t))
    (values (loop for (name . v) in slots
                  collect (cons name
                                (if (typep v 'va.types:Date)
                                    (or (days-ahead v (context-today *context*)) (setf ok nil))
                                    v)))
            ok)))

(defn best-match ((ws words) (fallback boolean)) (or null action)
  "The command whose template matches with the fewest ignored words, ties
going to the earlier one."
  (let (best best-skipped)
    (dolist (c *commands* best)
      (when (eq (command-fallback c) fallback)
        (dolist (tpl (command-parsed c))
          (multiple-value-bind (skipped slots) (find-template tpl ws #'slot-parser)
            (when (and skipped (or (null best) (< skipped best-skipped)))
              (multiple-value-bind (slots ok) (resolve-dates slots)
                (let ((fields (and ok (funcall (command-parse c) slots (template-source tpl)))))
                  (when fields
                    (setf best (make-action c (if (eq fields t) nil fields))
                          best-skipped skipped)))))))))))

(defn parse-tier ((text string) (fallback boolean)) (or null action)
  "Only the catch-alls (FALLBACK) or only the rest. The question inside a
request beats a catch-all on the whole of it."
  (let* ((ws (normalize text))
         (core (unwrap-request ws)))
    (or (and (not fallback) (not (equal core ws)) (best-match core nil))
        (best-match ws fallback)
        (and (not fallback)
             (let ((act (dialogue-act core))) (and act (ambient act)))))))

(defn parse-text ((text string)) (or null action)
  (or (parse-tier text nil) (parse-tier text t)))

(defn perform ((a action)) reply
  "Run A. A plain string from the command is its speech."
  (let ((out (let ((*late* (late-p)))
               (apply (command-run (action-command a)) (action-fields a)))))
    (if (stringp out) (reply (command-kind (action-command a)) "" out) out)))

(defn camel ((s symbol)) string
  (remove #\- (string-capitalize (symbol-name s))))

(defn debug-value ((v t) (type t)) string
  "V of Coalton TYPE as Rust's Debug prints it."
  (if (and v (consp type) (eq (first type) 'coalton:Optional))
      (format nil "Some(~a)" (debug-value v (second type)))
      (debug-plain v type)))

(defn debug-plain ((v t) (type t)) string
  (typecase v
    (null (if (eq type 'coalton:Boolean) "false" "None"))
    ((eql t) "true")
    ;; Coalton constructors without fields, like Power's Poweroff.
    (standard-object
     (let ((name (symbol-name (type-of v))))
       (string-capitalize (subseq name (1+ (position #\/ name))))))
    (string (format nil "~s" v))
    (float (if (= v (ftruncate v)) (format nil "~,1f" v) (format nil "~f" v)))
    (otherwise (princ-to-string v))))

(defn action-debug ((a (or null action))) string
  "As Rust's golden files show it: \"Fun(Dice(20))\", \"-\" for none."
  (if (null a)
      "-"
      (let* ((c (action-command a))
             (values (loop for v in (action-fields a)
                           for (nil type) in (command-fields c)
                           collect (debug-value v type)))
             (inner (cond ((null values) (command-kind c))
                          ((command-named c)
                           (format nil "~a { ~{~a: ~a~^, ~} }" (command-kind c)
                                   (loop for (name) in (command-fields c) for v in values
                                         collect (substitute #\_ #\- (string-downcase name))
                                         collect v)))
                          (t (format nil "~a(~{~a~^, ~})" (command-kind c) values)))))
        (if (command-bare c) inner (format nil "~a(~a)" (camel (command-skill c)) inner)))))

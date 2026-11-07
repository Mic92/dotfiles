(in-package #:va)

;;; The intent classifier's half in the brain: which sentences it learns
;;; from, and what to make of the nearest one. Rust only embeds and compares.

(defparameter *classify-threshold* 0.91d0
  "How close the nearest example must be.")

(defparameter *classify-margin* 0.02d0
  "How far ahead of the runner-up kind it must be.")

(defn classifiable ((a action)) boolean
  "Questions and harmless requests. Opposites embed close together (\"too
loud\", \"louder\"), so volume, players, power and toggles stay grammar-only."
  (let* ((c (action-command a)) (kind (command-kind c)))
    (flet ((one-of (&rest kinds) (and (member kind kinds :test #'string=) t)))
      (case (command-skill c)
        ((chat fun info calc news) t)
        (core (eq (command-name c) 'repeat))
        (ops (not (one-of "Bluetooth")))
        (media (one-of "NowPlaying"))
        (timer (one-of "Left" "Start"))
        (meeting (not (one-of "Join" "Create")))
        (notes (one-of "Show" "Contains"))
        (reminder (one-of "List"))
        (otherwise nil)))))

(defparameter *example-pool*
  '("five minutes" "ten minutes" "one hour" "7 pm" "half past six" "noon" "tomorrow" "today"
    "tomorrow morning" "this evening" "3" "20" "50" "munich" "berlin" "milk" "milk and eggs"
    "shopping" "firefox" "terminal" "two plus two" "something" "the dishes" "wifi")
  "Slot values to try in example sentences.")

(defn example-values ((slot string)) list
  "A few values for SLOT in example sentences."
  (let ((closed (closed-values slot)))
    (if closed
        (mapcar #'unwords (subseq closed 0 (min 3 (length closed))))
        (let ((parse (slot-parser slot)))
          (and parse
               (loop for v in *example-pool*
                     when (funcall parse (split-words v) nil) collect v into ok
                     until (= (length ok) 3)
                     finally (return ok)))))))

(defn classifier-examples () list
  "Grammar sentences for the classifier as (text kind slotted), SLOTTED when
`fill-slots` must find the values again."
  (let ((out (loop for c in (commands)
                   unless (command-fallback c)
                   append (loop for tm in (command-parsed c)
                                unless (has-open-slots tm)
                                append (loop with slotted = (has-slots tm)
                                             for s in (expand tm #'example-values 40)
                                             for a = (parse-tier s nil)
                                             when (and a (classifiable a))
                                             collect (list s (action-kind (action-debug a))
                                                           slotted))))))
    (remove-duplicates (stable-sort out #'string< :key #'first)
                       :key #'first :test #'string= :from-end t)))

(defn fill-slots ((text string) (kind string)) (or null action)
  "An action of KIND with slot values found anywhere in TEXT, the one using
the most slots."
  (let ((ws (normalize text)) best (most -1))
    (dolist (c (commands) best)
      (unless (command-fallback c)
        (dolist (tm (command-parsed c))
          (when (has-slots tm)
            (multiple-value-bind (found slots) (slots-anywhere tm ws #'slot-parser)
              (multiple-value-bind (slots ok) (resolve-dates slots)
                (let* ((fields (and found ok (funcall (command-parse c) slots
                                                      (template-source tm))))
                       (a (and fields (make-action c (if (eq fields t) nil fields)))))
                  (when (and a (classifiable a)
                             (string= kind (action-kind (action-debug a)))
                             (>= (length slots) most))
                    (setf best a most (length slots))))))))))))

(defn plain-action ((kind string)) (or null action)
  "What KIND means said without slot values."
  (let ((e (find-if (lambda (e) (and (string= kind (second e)) (not (third e))))
                    (classifier-examples))))
    (and e (parse-tier (first e) nil))))

(defparameter *change-words*
  '("add" "put" "remove" "removed" "delete" "erase" "take" "forget" "cross")
  "Words asking to change a list rather than show it.")
(defparameter *asking-words*
  '("did" "have" "is" "are" "do" "does" "what" "what's" "which" "has")
  "First words of a question about a list, which may mention changes.")
(defparameter *holiday-words* '("holiday" "holidays" "off" "free" "weekend")
  "Upcoming days off.")
(defparameter *not-holiday-words*
  '("paid" "pay" "payday" "taken" "took" "used" "left" "status" "request" "balance")
  "Pay or leave already taken, which the holiday command doesn't know.")

(defn vetoed ((kind string) (text string)) boolean
  "Sentences the classifier must not answer with KIND, though they embed close
to it. \"Add eggs and remove cheese\" lands next to \"is cheese on the list\",
but showing the list would ignore what was asked."
  (let ((ws (normalize text)))
    (flet ((has (set) (and (intersection ws set :test #'string=) t)))
      (cond ((a:starts-with-subseq "Info(Holiday" kind)
             (or (not (has *holiday-words*)) (has *not-holiday-words*)))
            ((or (a:starts-with-subseq "Notes(" kind) (a:starts-with-subseq "Reminder(" kind))
             (and (not (member (first ws) *asking-words* :test #'string=))
                  (has *change-words*)))
            (t nil)))))

(defn classified-action ((text string) (kind string) (score real) (lead real))
    (values (or null action) boolean)
  "The action the classifier's nearest KIND stands for in TEXT, and whether
it is sure enough to take it."
  (let ((a (or (fill-slots text kind) (plain-action kind))))
    (values a (and a (>= score *classify-threshold*) (>= lead *classify-margin*)
                   (not (vetoed kind text))))))

(defn classified ((text string) (kind string) (score real) (lead real)) hash-table
  (multiple-value-bind (a ok) (classified-action text kind score lead)
    (object "type" "classified" "action" (if a (action-debug a) 'null) "accepted" ok)))

(defn guessed ((text string) (guess (or null hash-table))) (or null action)
  "The action the classifier's GUESS (kind, score, lead) stands for, if sure."
  (when guess
    (multiple-value-bind (a ok)
        (classified-action text (message-field guess "kind" 'string)
                           (message-field guess "score" 'real) (message-field guess "lead" 'real))
      (and ok a))))

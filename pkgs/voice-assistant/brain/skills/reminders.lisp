;;; Spoken reminders as transient systemd user timers, so they survive the
;;; assistant restarting.

(in-package #:va)

(in-skill reminder)

(defparameter *reminder-unit* "va-reminder-"
  "Prefix of our reminder timers. The notification's twin starts va-notify-.")

(defn reminder-text ((text (or null string))) (or null string)
  "The open slot swallows the on of \"on monday\"."
  (and text (if (uiop:string-suffix-p text " on") (subseq text 0 (- (length text) 3)) text)))

(defn remind-seconds ((duration (or null (integer 0))) (clock (or null va.types:Clock))
                      (day (or null (integer 0))))
    (or null (integer 0))
  (cond (duration duration)
        ((and clock day) (seconds-on clock (now) day))
        (clock (seconds-from clock (now)))))

(defparameter *reminder-fillers*
  '("the" "a" "an" "my" "to" "about" "for" "of" "that" "reminder" "me" "i")
  "Words that do not tell reminders apart.")

(defn content ((text string)) list
  (remove-if (lambda (w) (member w *reminder-fillers* :test #'string=)) (normalize text)))

(defn spoken-delay ((seconds (integer 0))) string
  (multiple-value-bind (h m) (floor (ceiling seconds 60) 60)
    (let ((hours (if (= h 1) "an hour" (format nil "~d hours" h))))
      (cond ((< seconds 60) (format nil "~d seconds" seconds))
            ((and (zerop h) (= m 1)) "a minute")
            ((zerop h) (format nil "~d minutes" m))
            ((zerop m) hours)
            (t (format nil "~a and ~d minutes" hours m))))))

(defn epoch () (or null (integer 0))
  "Seconds since 1970, from date so tests can fake it."
  (let ((s (run "date" "+%s"))) (and s (parse-integer s :junk-allowed t))))

(defreplies *reminder-set*
  "Okay, in ~a, I'll nag you." "Got it. ~a from now, you'll hear from me."
  "Fine. I'll remind you in ~a. Don't make me repeat myself."
  "In ~a, I'll be in your ear. Like a conscience, but louder.")

(defn remind ((what string) (seconds (integer 0))) reply
  (let ((id (or (let ((s (run "date" "+%s%N"))) (and s (trim s)))
                (princ-to-string (random 1000000))))
        (when (format nil "--on-active=~ds" seconds))
        (msg (format nil "Reminder: ~a" what))
        (delay (spoken-delay seconds)))
    (spawn "systemd-run" "--user" "--collect" "--quiet" (format nil "--unit=va-notify-~a" id) when
           "notify-send" "-u" "critical" "Reminder" what)
    (spawn "systemd-run" "--user" "--collect" "--quiet"
           (format nil "--unit=~a~a" *reminder-unit* id) (format nil "--description=~a" msg) when
           "voice-assistant" "--say" msg)
    (live "Reminder" (format nil "~a in ~a" what delay) (pick *reminder-set* delay))))

(defn pending ((raw string) (now (integer 0))) list
  "(unit . seconds left) of reminder timers in systemctl list-timers JSON,
soonest first. Elapsed ones have next 0."
  (let ((timers (parse-json raw)))
    (sort (and (vectorp timers) (not (stringp timers))
               (loop for timer across timers
                     for unit = (json-get timer "unit")
                     for next = (json-get timer "next")
                     when (and (stringp unit) (realp next) (>= (floor next 1000000) now))
                     collect (cons unit (- (floor next 1000000) now))))
          #'< :key #'cdr)))

(defstruct (pending (:constructor make-pending (unit what left)))
  "A pending reminder: its timer unit, text and seconds left."
  (unit "" :type string) (what "" :type string) (left 0 :type (integer 0)))

(defn pending-now () list
  (let ((raw (run "systemctl" "--user" "list-timers" (format nil "~a*" *reminder-unit*)
                  "--output=json" "--all"))
        (now (epoch)))
    (and raw now
         (loop for (unit . left) in (pending raw now)
               for what = (trim (or (run "systemctl" "--user" "show" unit "-p" "Description"
                                         "--value")
                                    ""))
               collect (make-pending unit (if (uiop:string-prefix-p "Reminder: " what)
                                              (subseq what 10)
                                              what)
                                     left)))))

(defreplies *no-reminders*
  "No reminders. Your memory is on its own."
  "Nothing pending. Enjoy the freedom while it lasts."
  "None. Either you're very organised or very forgetful."
  "Zero. I have nothing to nag you about. I hate it.")
(defreplies *reminders*
  "~a." "Here's what you made me remember: ~a."
  "~a. And no, I won't forget. Unlike some people."
  "Coming up: ~a. Don't say I didn't warn you.")

(defn list-reminders () reply
  (let ((items (mapcar (lambda (p) (format nil "~a in ~a" (pending-what p)
                                           (spoken-delay (pending-left p))))
                       (pending-now))))
    (if (null items)
        (live "Reminders" "" (pick *no-reminders*))
        (let ((what (spoken-list items))) (live "Reminders" what (pick *reminders* what))))))

(defn matching ((want (or null string)) (whats list)) list
  "The reminders sharing the most words with WANT, or all of them without a
name. \"dentist\" also finds \"go to the dentists\"."
  (if (null want)
      whats
      (let* ((want (content want))
             (scores (mapcar (lambda (what)
                               (let ((have (content what)))
                                 (count-if (lambda (w)
                                             (some (lambda (h) (or (uiop:string-prefix-p w h)
                                                                   (uiop:string-prefix-p h w)))
                                                   have))
                                           want)))
                             whats))
             (best (reduce #'max scores :initial-value 0)))
        (and (plusp best) (loop for w in whats for s in scores when (= s best) collect w)))))

(defn stop-reminder ((unit string)) null
  "One unit per call: a missing notify timer must not keep the reminder alive."
  (dolist (u (list unit (cl-ppcre:regex-replace *reminder-unit* unit "va-notify-")))
    (run "systemctl" "--user" "stop" u)))

(defreplies *reminder-cancelled*
  "Done, no more ~a. I'll pretend you never asked."
  "Forgotten: ~a. Like your gym membership."
  "Cancelled ~a. Your future self owes me nothing." "~a? Gone. Poof."
  "Scrapped ~a. Living dangerously, I see.")
(defreplies *nothing-to-cancel*
  "There's nothing to cancel. You're free as a bird."
  "No reminders pending. Nothing to forget."
  "Cancel what? I'm not nagging you about anything.")
(defreplies *no-such-reminder*
  "I have no reminder about ~a." "~a? Never heard of it. Maybe you forgot to ask."
  "Nothing about ~a. Your memory is worse than mine.")

(defn which-reminder () question
  "Which of several to cancel: yes or \"both\" means all of them."
  (question :act (lambda (act) (and (eq act :yes) (command-action 'reminder-cancel nil t)))
            :answer (lambda (text)
                      (let ((all (some (lambda (w) (in w "all" "both" "every")) (normalize text))))
                        (command-action 'reminder-cancel (and (not all) text) all)))))

(defn cancel-reminders ((want (or null string)) (all boolean)) reply
  (let* ((pending (pending-now))
         (hits (matching want (mapcar #'pending-what pending))))
    (cond ((null pending) (live "Reminders" "" (pick *nothing-to-cancel*)))
          ((null hits) (live "Reminders" (or want "") (pick *no-such-reminder* (or want ""))))
          ((or all (null (rest hits)))
           (dolist (p pending)
             (when (member (pending-what p) hits :test #'string=) (stop-reminder (pending-unit p))))
           (let ((what (spoken-list hits)))
             (live "Reminders" what (pick *reminder-cancelled* what))))
          (t (let ((options (cl-ppcre:regex-replace " and " (spoken-list hits) " or ")))
               (reply "Reminders" options (format nil "Which one, ~a?" options) :live t
                      :question (which-reminder)))))))

(defreplies *ask-what*
  "Remind you of what?" "Sure. What shall I nag you about?"
  "What do you want me to remember? Since you won't." "Go on, what is it?")
(defreplies *ask-remind-when*
  "When should I remind you to ~a?" "~a. And when?"
  "When? Give me a time or I'll pick a terrible one." "When do you want to be nagged?")
(defreplies *ask-tomorrow*
  "Tomorrow at what time?" "Tomorrow when? Before or after your Club-Mate?")

(defn ask-what ((day (or null (integer 0)))) function
  "\"What shall I remind you of?\""
  (lambda (text)
    (let ((ws (strip-one (normalize text) "to" "about" "that" "of")))
      (and ws (command-action 'reminder-ask (unwords ws) day)))))

(defn answer-day ((ws words) (day (or null (integer 0))))
    (values (or null (integer 0)) words boolean)
  "The day before \"at\" in \"friday at 9\", and the words from \"at\" on.
Without one, DAY and WS. The last value is false when the day was not one."
  (let ((at (position "at" ws :test #'string=)))
    (if (and at (plusp at))
        (let ((said (days-from-today (subseq ws 0 at))))
          (values said (nthcdr at ws) (and said t)))
        (values day ws t))))

(defn ask-when ((what string) (day (or null (integer 0)))) function
  "\"When should I remind you?\": \"in 20 minutes\", \"at 5\", \"tomorrow at 9\"."
  (lambda (text)
    (multiple-value-bind (day ws ok) (answer-day (normalize text) day)
      (let* ((ws (strip-one ws "in" "at" "for"))
             (duration (and (null day) (funcall (slot-parser "duration") ws nil)))
             (clock (and (null duration) (parse-clock ws)))
             (seconds (and ok (remind-seconds duration clock day))))
        (and seconds (command-action 'reminder-set what seconds))))))

(defn ask-reminder ((what (or null string)) (day (or null (integer 0)))) reply
  (if (null what)
      (reply "Reminder" "" (pick *ask-what*) :question (question :answer (ask-what day)))
      (reply "Reminder" what (pick (if (eql day 1) *ask-tomorrow* *ask-remind-when*) what)
             :question (question :answer (ask-when what day)))))

(defcommand reminder-cancel
  "![please] (cancel|delete|remove|clear|drop|stop) [the|my|that] reminder (about|for|to) {text}"
  "![please] (cancel|delete|remove|clear|drop) [the|my] {text} reminder"
  "!(don't|do not) remind me (to|about|of) {text} (anymore|any more)"
  "!(don't|do not) remind me (to|about|of) {text}"
  "!forget (the|my|that) reminder (about|for|to) {text}"
  :kind "Cancel"
  :named t
  :slots ((what (coalton:Optional coalton:String)) (all coalton:Boolean))
  ;; "cancel that reminder" names none.
  :parse (let ((text (slot "text")))
           (list (and text (notevery (lambda (w) (in w "this" "last")) (content text)) text) nil))
  :examples ("cancel the reminder about the dentist")
  (cancel-reminders what all))

(defcommand reminder-cancel-one
  "![please] (cancel|delete|remove|clear|drop|stop) [the|my|that|this] reminder"
  "!forget (the|my|that) reminder"
  :kind "Cancel"
  :named t
  :slots ((what (coalton:Optional coalton:String)) (all coalton:Boolean))
  :parse (list nil nil)
  :examples ("cancel that reminder")
  (cancel-reminders what all))

(defcommand reminder-cancel-all
  "![please] (cancel|delete|remove|clear|drop) (all|every) [of] [my|the] reminders"
  :kind "Cancel"
  :named t
  :slots ((what (coalton:Optional coalton:String)) (all coalton:Boolean))
  :parse (list nil t)
  :examples ("delete all my reminders")
  (cancel-reminders what all))

;; A time first: open text would swallow "call mum in 10 minutes".
(defcommand reminder-set
  "![can you|could you|please] remind me (to|about|that) {text} in {duration}"
  (:join "![can you|could you|please] (set|make|create|add|set up) [me] [a] reminder [for me]"
         "(to|about|that|for) {text} in {duration}")
  "!(i need|i want) [a] reminder (to|about|that|for) {text} in {duration}"
  (:join "!(i need|i want|i would like|i'd like) to (set|make|set up) [a] [new] reminder [for"
         "myself] (to|about|that|for) {text} in {duration}")
  "!(i need|i want) to (be|get) reminded (to|about|of) {text} in {duration}"
  "![can you] give me a reminder (to|about|for) {text} in {duration}"
  "![can you|could you|please] remind me in {duration} (to|about|that) {text}"
  :kind "Set"
  :named t
  :slots ((what coalton:String) (seconds coalton:UFix))
  :parse (let ((what (reminder-text (slot "text"))))
           (and what (list what (slot "duration"))))
  :examples ("remind me to call mum in 10 minutes" "remind me in an hour to check the oven")
  (remind what seconds))

(defcommand reminder-set-at
  "![can you|could you|please] remind me (to|about|that) {text} {day} at {clock}"
  (:join "![can you|could you|please] (set|make|create|add|set up) [me] [a] reminder [for me]"
         "(to|about|that|for) {text} {day} at {clock}")
  "!(i need|i want) [a] reminder (to|about|that|for) {text} {day} at {clock}"
  (:join "!(i need|i want|i would like|i'd like) to (set|make|set up) [a] [new] reminder [for"
         "myself] (to|about|that|for) {text} {day} at {clock}")
  "!(i need|i want) to (be|get) reminded (to|about|of) {text} {day} at {clock}"
  "![can you] give me a reminder (to|about|for) {text} {day} at {clock}"
  "![can you|could you|please] remind me {day} at {clock} (to|about|that) {text}"
  "![can you|could you|please] remind me (to|about|that) {text} at {clock} {day}"
  (:join "![can you|could you|please] (set|make|create|add|set up) [me] [a] reminder [for me]"
         "(to|about|that|for) {text} at {clock} {day}")
  "!(i need|i want) [a] reminder (to|about|that|for) {text} at {clock} {day}"
  (:join "!(i need|i want|i would like|i'd like) to (set|make|set up) [a] [new] reminder [for"
         "myself] (to|about|that|for) {text} at {clock} {day}")
  "!(i need|i want) to (be|get) reminded (to|about|of) {text} at {clock} {day}"
  "![can you] give me a reminder (to|about|for) {text} at {clock} {day}"
  "![can you|could you|please] remind me at {clock} {day} (to|about|that) {text}"
  "![can you|could you|please] remind me (to|about|that) {text} at {clock}"
  (:join "![can you|could you|please] (set|make|create|add|set up) [me] [a] reminder [for me]"
         "(to|about|that|for) {text} at {clock}")
  "!(i need|i want) [a] reminder (to|about|that|for) {text} at {clock}"
  (:join "!(i need|i want|i would like|i'd like) to (set|make|set up) [a] [new] reminder [for"
         "myself] (to|about|that|for) {text} at {clock}")
  "!(i need|i want) to (be|get) reminded (to|about|of) {text} at {clock}"
  "![can you] give me a reminder (to|about|for) {text} at {clock}"
  "![can you|could you|please] remind me at {clock} (to|about|that) {text}"
  :kind "Set"
  :named t
  :slots ((what coalton:String) (seconds coalton:UFix))
  :parse (let ((what (reminder-text (slot "text")))
               (seconds (remind-seconds nil (slot "clock") (day-offset (slot "day")))))
           (and what seconds (list what seconds)))
  :examples ("remind me at 5 pm to check the oven" "remind me to call mum tomorrow at 9")
  (remind what seconds))

(defcommand reminder-ask
  "![can you|could you|please] remind me (to|about|that) {text} {day}"
  (:join "![can you|could you|please] (set|make|create|add|set up) [me] [a] reminder [for me]"
         "(to|about|that|for) {text} {day}")
  "!(i need|i want) [a] reminder (to|about|that|for) {text} {day}"
  (:join "!(i need|i want|i would like|i'd like) to (set|make|set up) [a] [new] reminder [for"
         "myself] (to|about|that|for) {text} {day}")
  "!(i need|i want) to (be|get) reminded (to|about|of) {text} {day}"
  "![can you] give me a reminder (to|about|for) {text} {day}"
  "![can you|could you|please] remind me {day} (to|about|that) {text}"
  "![can you|could you|please] remind me (to|about|that) {text}"
  (:join "![can you|could you|please] (set|make|create|add|set up) [me] [a] reminder [for me]"
         "(to|about|that|for) {text}")
  "!(i need|i want) [a] reminder (to|about|that|for) {text}"
  (:join "!(i need|i want|i would like|i'd like) to (set|make|set up) [a] [new] reminder [for"
         "myself] (to|about|that|for) {text}")
  "!(i need|i want) to (be|get) reminded (to|about|of) {text}"
  "![can you] give me a reminder (to|about|for) {text}"
  "![can you|could you|please] remind me (to|about|that) {text}"
  :kind "Ask"
  :named t
  :slots ((what (coalton:Optional coalton:String)) (day (coalton:Optional coalton:UFix)))
  :parse (list (reminder-text (slot "text")) (day-offset (slot "day")))
  :examples ("remind me to pay the mortgage" "remind me to let the plumber in tomorrow")
  (ask-reminder what day))

(defcommand reminder-ask-bare
  (:join "![can you|could you|will you|please] (set|make|create|add|give|set up) [me] [a]"
         "reminder [for me] [please]")
  "!(i need|i want|i would like|i'd like) [to set|to make] [a] reminder [please]"
  "![please] remind me [later] [please]"
  "![please] set [a] new reminder"
  "!(i need|i want) to (set|set up|make) [a] reminder [for myself]"
  "!can i set a reminder"
  :kind "Ask"
  :named t
  :slots ((what (coalton:Optional coalton:String)) (day (coalton:Optional coalton:UFix)))
  :parse (list nil nil)
  :examples ("set a reminder" "remind me")
  (ask-reminder what day))

(defcommand reminder-list
  "!(what|which) reminders (do|did) i have [set]"
  "!(what|which) reminders have i set"
  "!what are my [current] reminders"
  "!do i have any reminders"
  "![tell me] (what's|what is) on my (reminder|reminders) list [today]"
  (:join "![can you] (read|tell|show|list|repeat) [me] [all] my [current] (reminders|reminder"
         "list|reminders list|list of reminders) [to me|back to me]")
  "![can you] tell me (what reminders|the reminders) i (have|i've) set [up]"
  "![can you] tell me what reminders i've set"
  "!read back [the|my] reminder list"
  "!what (did|do) i (want|need|ask you) to remember [again]"
  "!what did i ask to be reminded (about|of)"
  :kind "List"
  :examples ("what reminders do i have" "read me my reminders")
  (list-reminders))

(defskill reminder)

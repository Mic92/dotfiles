;;; One conversation with the Rust ears: what was said last, the question
;;; waiting for an answer and whether we are asleep. The heard message
;;; turns one utterance into what to say and do.

(in-package #:va)

(defconstant +carry-over+ 120
  "Seconds \"and tomorrow?\" may refer back to the previous command.")

(defstruct (conversation (:constructor make-conversation ()))
  "What the next utterance is heard against. ASLEEP is t until \"wake up\",
or the second (days since 1970 times 86400 plus local time) a nap ends."
  (previous nil :type (or null action))
  (previous-at 0 :type integer)
  (question nil :type (or null question))
  (misses 0 :type (integer 0))
  (last-speech "" :type string)
  (asleep nil :type (or boolean integer)))

(defvar *conversation* (make-conversation)
  "The conversation with the one Rust client.")

(defvar *conversation-lock* (sb-thread:make-mutex :name "conversation")
  "Held while an utterance is heard, so two never interleave.")

(defreplies *not-understood*
  "~a? Sorry, I didn't catch that." "~a? Come again? Actually, don't answer that."
  "I heard ~a. Words, yes. Useful ones, no."
  "~a? Was that English? Try again, slower. And sexier."
  "~a? No idea what that means, but it sounded dirty."
  "You said ~a? Nope. Try again, with feeling." "~a? I'm an assistant, not a mind reader."
  "Did you just say ~a? Bold of you." "~a? I'll pretend you didn't say that."
  "~a? That's not a command, that's a confession.")
(defreplies *still-not*
  "~a? Still nope. Are you doing this on purpose?"
  "Again with the ~a. We're going in circles, babe." "~a? I'm trying, okay? Use small words."
  "~a? Again? Darling, enunciate.")
(defreplies *give-up*
  "~a. Okay, I give up. Maybe type it?" "~a. Let's just pretend this never happened."
  "I'm starting to think ~a is a cry for help." "~a. I'm going to need a minute.")
(defreplies *not-heard-again*
  "Still mumbling. Is there a sock in your mouth?"
  "Nope, nothing. Did you forget how words work?"
  "Still nothing. Is the microphone shy, or are you?")
(defreplies *not-heard*
  "Sorry, I didn't catch that." "Mumbling is not a command, sweetie."
  "Speak up, I don't bite. Much." "Was that a whisper? Come closer."
  "I heard breathing. That's a bit forward.")

(defn not-understood ((heard string) (misses (integer 0))) string
  "Quotes what we heard back when it is short enough to be worth repeating.
MISSES counts failures in a row, so patience can run out."
  (let* ((heard (string-right-trim ".!?," (trim heard)))
         (n (length (split-words heard))))
    (if (or (zerop n) (> n 12))
        (choose *not-heard* nil (>= misses 2) *not-heard-again*)
        (let ((line (choose *not-understood* heard (>= misses 3) *give-up*
                            (= misses 2) *still-not*)))
          (string-capitalize line :end (min 1 (length line)))))))

(defn action-kind ((debug string)) string
  "The command without its fields, as heard.tsv logs it: \"Fun(Dice\"."
  (let ((cut (loop for i from 0 below (length debug)
                   for c = (char debug i)
                   with parens = 0
                   when (find c " {\"") return i
                   when (char= c #\() do (incf parens)
                   (when (= parens 2) (return i)))))
    (subseq debug 0 (or cut (length debug)))))

(defn named-p ((a action) &rest (names symbol)) boolean
  (and (member (command-name (action-command a)) names) t))

(defn heard-reply (&rest (fields json)) hash-table
  "A heard reply, defaults filled in for whatever FIELDS leave out."
  (let ((h (apply #'object "type" "heard" fields)))
    (loop for (k v) on '("outcome" "command" "action" null "kind" null "title" "" "body" ""
                         "speech" "" "cache" nil "chime" null "notify" nil "voice" null
                         "leave_paused" nil "question" nil)
          by #'cddr
          do (unless (nth-value 1 (gethash k h)) (setf (gethash k h) v)))
    h))

(defn quiet ((outcome string)) hash-table
  (heard-reply "outcome" outcome "chime" "cancel"))

(defn miss ((text string)) hash-table
  (let ((speech (not-understood text (incf (conversation-misses *conversation*)))))
    (setf (conversation-last-speech *conversation*) speech)
    (heard-reply "outcome" "miss" "title" "Didn't understand" "speech" speech
                 "chime" "error" "notify" t)))

(defn awake-p ((at integer)) boolean
  "Whether we listen at AT, ending a nap that is over."
  (let ((asleep (conversation-asleep *conversation*)))
    (when (and (integerp asleep) (>= at asleep))
      (setf (conversation-asleep *conversation*) nil))
    (not (conversation-asleep *conversation*))))

(defn remember ((a action) (r reply) (at integer)) null
  "What A changes about the conversation, after saying R."
  (let ((c *conversation*))
    (cond ((named-p a 'listen-off) (setf (conversation-asleep c) t))
          ((named-p a 'listen-on) (setf (conversation-asleep c) nil))
          ((named-p a 'nap) (setf (conversation-asleep c) (+ at (first (action-fields a))))))
    (setf (conversation-last-speech c) (reply-speech r)
          (conversation-question c) (reply-question r)
          (conversation-misses c) 0)
    (unless (named-p a 'repeat 'dismiss 'say 'voice-on 'voice-off 'listen-on 'listen-off)
      (setf (conversation-previous c) a
            (conversation-previous-at c) at))
    nil))

(defn performed ((a action)) reply
  "A's reply. \"repeat that\" says the last thing again, and a skill that
breaks says so instead of taking the brain down."
  (if (named-p a 'repeat)
      (let ((last (conversation-last-speech *conversation*)))
        (reply "" "" (if (string= last "") "I haven't said anything yet. Shy?" last)))
      (handler-case (perform a)
        (error (e)
          (format *error-output* "~&va-brain: ~a failed: ~a~%" (action-debug a) e)
          (reply "Assistant" (princ-to-string e) "That broke. Not my fault, obviously.")))))

(defn run-heard ((a action) (at integer)) hash-table
  (let ((filler (command-filler (action-command a))))
    (when (and filler *send*)
      (funcall *send* (object "type" "filler" "speech" (pick filler)))))
  (let ((r (performed a)) (debug (action-debug a)))
    (remember a r at)
    (heard-reply "action" debug "kind" (action-kind debug)
                 "title" (if (string= (reply-title r) "") "Assistant" (reply-title r))
                 "body" (reply-body r) "speech" (reply-speech r)
                 "cache" (not (reply-live r))
                 "chime" (if (named-p a 'dismiss) "cancel" "success")
                 "notify" t
                 "voice" (cond ((named-p a 'voice-on) t) ((named-p a 'voice-off) nil) (t 'null))
                 "leave_paused" (and (eq (command-skill (action-command a)) 'media)
                                     (not (named-p a 'media-now-playing)))
                 "question" (and (reply-question r) t))))

(defn heard-action ((text string) (answering boolean) (cut-off boolean) (unsure boolean)
                    (dry boolean))
    (or null action (eql :unsure))
  "The strict reading, answering the open question when ANSWERING, then the
catch-alls unless the listening was cut off. UNSURE leaves the catch-alls to
a second request after rescoring, and the question open until then. DRY
only reads, keeping the question too."
  (let* ((c *conversation*)
         (strict (respond text (and answering (conversation-question c)))))
    (cond ((and unsure (null strict) (not cut-off)) :unsure)
          (t (unless dry (setf (conversation-question c) nil))
             (or strict (and (not cut-off) (parse-tier text t)))))))

(defn hear-text ((msg hash-table)) hash-table
  (let* ((text (message-field msg "text" 'string))
         (at (+ (* 86400 (message-field msg "today" 'integer))
                (message-field msg "now" '(integer 0 86400))))
         (c *conversation*)
         (*context* (make-context
                     (message-field msg "now" '(integer 0 86400))
                     (message-field msg "today" 'integer)
                     :previous (and (< (- at (conversation-previous-at c)) +carry-over+)
                                    (conversation-previous c))
                     :timer (eq t (gethash "timer" msg))
                     :music (eq t (gethash "music" msg)))))
    (if (and (not (awake-p at)) (not (named-p-text text 'listen-on)))
        (quiet "asleep")
        (let ((a (heard-action text (eq t (gethash "answering" msg))
                               (eq t (gethash "cut_off" msg)) (eq t (gethash "unsure" msg))
                               (eq t (gethash "dry" msg)))))
          (cond ((eq a :unsure) (heard-reply "outcome" "unsure"))
                ((and (null a) (eq t (gethash "cut_off" msg))) (quiet "noise"))
                ((null a) (miss text))
                ((eq t (gethash "dry" msg))
                 (heard-reply "action" (action-debug a) "kind" (action-kind (action-debug a))))
                (t (run-heard a at)))))))

(defn named-p-text ((text string) (name symbol)) boolean
  (let ((a (parse-tier text nil))) (and a (named-p a name))))

(defn hear ((msg hash-table)) hash-table
  "The reply to one utterance, heard one at a time."
  (sb-thread:with-mutex (*conversation-lock*) (hear-text msg)))

;;; The heard message: one utterance in, what to say and do out.

(in-package #:va-tests)

(defvar *sent* nil "Messages the brain sent before its reply, newest first.")

(defun hear (text &rest fields)
  "The brain's reply to hearing TEXT at 10:00 on Wednesday 2025-10-22, or as
FIELDS, alternating names and values, say."
  (let ((msg (make-hash-table :test 'equal)))
    (loop for (k v) on (append (list "id" 7 "type" "heard" "text" text "now" (* 10 3600)
                                     "today" 20383)
                               fields)
          by #'cddr
          do (setf (gethash k msg) v))
    (let ((va::*send* (lambda (m) (push m *sent*))))
      (va:fake-effects-value (va::answer msg)))))

(defun got (reply key) (gethash key reply))

(defmacro conversation (&body body)
  "BODY with a fresh conversation."
  `(let ((va::*conversation* (va::make-conversation)) (*sent* nil))
     ,@body))

(define-test heard-commands
  :parent va-tests
  (conversation
   (let ((r (hear "roll a die")))
     (is equal "heard" (got r "type"))
     (is equal 7 (got r "id"))
     (is equal "command" (got r "outcome"))
     (is equal "Fun(Dice(6))" (got r "action"))
     (is equal "Fun(Dice" (got r "kind"))
     (is equal "success" (got r "chime"))
     (true (got r "notify"))
     (false (got r "question"))
     (true (plusp (length (got r "speech"))))
     (is equal (got r "speech") (got (hear "repeat that") "speech")))))

(define-test heard-fillers-come-first
  :parent va-tests
  (conversation
   (hear "tell me a joke")
   (is equal '("filler") (mapcar (lambda (m) (gethash "type" m)) *sent*))
   (is equal 7 (gethash "id" (first *sent*)))))

(define-test heard-misses
  :parent va-tests
  (conversation
   (let ((r (hear "blorp the frobnicator")))
     (is equal "miss" (got r "outcome"))
     (is eq 'null (got r "action"))
     (is equal "error" (got r "chime"))
     (is equal "Didn't understand" (got r "title"))
     (true (search "blorp the frobnicator" (string-downcase (got r "speech"))))
     (true (upper-case-p (char (got r "speech") 0))))
   (is equal "noise" (got (hear "blorp blorp blorp" "cut_off" t) "outcome"))
   (is equal "command" (got (hear "roll a die" "cut_off" t) "outcome"))))

(define-test heard-patience-runs-out
  :parent va-tests
  (conversation
   (hear "bake a cake")
   (hear "bake a cake")
   (let ((third (got (hear "bake a cake") "speech")))
     (true (some (lambda (l) (string-equal (format nil l "bake a cake") third))
                 (va::replies-lines va::*give-up*))))))

(define-test heard-sleep
  :parent va-tests
  (conversation
   (is equal "command" (got (hear "stop listening") "outcome"))
   (let ((r (hear "roll a die")))
     (is equal "asleep" (got r "outcome"))
     (is equal "cancel" (got r "chime"))
     (false (got r "notify")))
   (is equal "command" (got (hear "wake up") "outcome"))
   (is equal "command" (got (hear "roll a die") "outcome")))
  (conversation
   (hear "take a nap for 10 minutes" "now" 36000)
   (is equal "asleep" (got (hear "roll a die" "now" 36500) "outcome"))
   (is equal "command" (got (hear "roll a die" "now" 36601) "outcome"))))

(define-test heard-voice
  :parent va-tests
  (conversation
   (is eq nil (got (hear "stop talking") "voice"))
   (is eq t (got (hear "you can talk again") "voice"))
   (is eq 'null (got (hear "roll a die") "voice"))))

(define-test heard-questions
  :parent va-tests
  (conversation
   (true (got (hear "add dentist to my calendar") "question"))
   (is equal "Meeting(Create { title: \"dentist\", day: 2, secs: 32400, confirmed: false })"
       (got (hear "friday at 9 am" "answering" t "dry" t) "action")))
  (conversation
   (hear "add dentist to my calendar")
   (isnt equal "command" (got (hear "friday at 9 am" "dry" t) "outcome")
         "without answering the question was not heard")))

(define-test heard-unsure-waits-for-rescoring
  :parent va-tests
  (conversation
   (hear "add dentist to my calendar")
   (let ((r (hear "flibber at nine" "answering" t "unsure" t)))
     (is equal "unsure" (got r "outcome"))
     (false (got r "notify")))
   (is equal "Meeting(Create { title: \"dentist\", day: 2, secs: 32400, confirmed: false })"
       (got (hear "friday at 9 am" "answering" t "dry" t) "action")
       "the question is still open")
   (is equal "Meeting(Create { title: \"dentist\", day: 2, secs: 32400, confirmed: false })"
       (got (hear "friday at 9 am" "answering" t "dry" t) "action")
       "dry runs change nothing"))
  (conversation
   (is equal "noise" (got (hear "flibber" "unsure" t "cut_off" t) "outcome"))
   (is equal "command" (got (hear "roll a die" "unsure" t) "outcome"))))

(define-test heard-takes-a-sure-classification
  :parent va-tests
  (flet ((guess (score)
           (va::object "kind" "Fun(Joke)" "score" score "lead" 0.1d0)))
    (conversation
     (is equal "Fun(Joke)" (got (hear "crack me up" "dry" t "classified" (guess 0.95d0)) "action"))
     (is equal "miss" (got (hear "crack me up" "classified" (guess 0.5d0)) "outcome"))
     (is equal "Fun(Joke)" (got (hear "tell a joke" "dry" t "classified" (guess 0.1d0)) "action")
         "the grammar comes first"))))

(define-test heard-follow-ups
  :parent va-tests
  (conversation
   (hear "what's on my calendar today" "now" 36000)
   (is equal "Meeting(Day { day: 1, part: None })"
       (got (hear "and tomorrow?" "now" 36060 "dry" t) "action"))
   (hear "what's on my calendar today" "now" 36000)
   (isnt equal "Meeting(Day { day: 1, part: None })"
         (got (hear "and tomorrow?" "now" 36200 "dry" t) "action"))))

(define-test heard-leaves-music-paused-for-media
  :parent va-tests
  (conversation
   (true (got (hear "pause the music" "music" t) "leave_paused"))
   (false (got (hear "roll a die" "music" t) "leave_paused"))))

(define-test heard-kinds
  :parent va-tests
  (is equal "Dismiss" (va::action-kind "Dismiss"))
  (is equal "Nap(600)" (va::action-kind "Nap(600)"))
  (is equal "Say(" (va::action-kind "Say(\"hi\")"))
  (is equal "Meeting(Create" (va::action-kind "Meeting(Create { title: \"x\" })"))
  (is equal "Fun(Dice" (va::action-kind "Fun(Dice(6))")))

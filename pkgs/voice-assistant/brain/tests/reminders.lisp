;;; Ported from reminders.rs.

(in-package #:va-tests)

(defmacro at-two-pm (&body body)
  "BODY at 14:00 on Wednesday 2025-10-22."
  `(let ((va::*context* (va::make-context (* 14 3600) (va::day-number 2025 10 22))))
     ,@body))

(defun set-reminder (what seconds)
  (format nil "Reminder(Set { what: ~s, seconds: ~d })" what seconds))

(defun ask-reminder (what day)
  (format nil "Reminder(Ask { what: ~:[None~;~:*Some(~s)~], day: ~:[None~;~:*Some(~d)~] })"
          what day))

(define-test reminders-cancel
  :parent va-tests
  (let ((whats '("call mum" "go to the dentists" "take out the trash")))
    (is equal '("go to the dentists") (va::matching "the dentist" whats))
    (is equal '("call mum") (va::matching "about calling mum" whats))
    (is equal '() (va::matching "pizza" whats))
    (is = 3 (length (va::matching nil whats))))
  (with-focus ()
    (parses-as "Reminder(Cancel { what: ~a, all: ~a })"
               '(("cancel the reminder about the dentist" "Some(\"the dentist\")" "false")
                 ("delete my dentist reminder" "Some(\"dentist\")" "false")
                 ("don't remind me to call mum anymore" "Some(\"call mum\")" "false")
                 ("cancel that reminder" "None" "false")
                 ("delete all my reminders" "None" "true")))
    (true (uiop:string-prefix-p "Timer" (parsed "cancel the timer")))))

(define-test reminders-pending
  :parent va-tests
  (is equal "30 seconds" (va::spoken-delay 30))
  (is equal "10 minutes" (va::spoken-delay 600))
  (is equal "an hour" (va::spoken-delay 3600))
  (is equal "an hour and 30 minutes" (va::spoken-delay 5400))
  (is equal "2 hours" (va::spoken-delay 7200))
  (is equal '(("va-reminder-1.timer" . 60) ("va-reminder-2.timer" . 600))
      (va::pending (concatenate 'string
                                "[{\"next\":1700000600000000,\"unit\":\"va-reminder-2.timer\"},"
                                "{\"next\":1700000060000000,\"unit\":\"va-reminder-1.timer\"},"
                                "{\"next\":0,\"unit\":\"va-reminder-old.timer\"}]")
                   1700000000)))

(define-test reminders-ask-for-what-is-missing
  :parent va-tests
  (at-two-pm
   (loop for (text want) in
         `(("remind me to call my mom" ,(ask-reminder "call my mom" nil))
           ("set a reminder to feed the dogs" ,(ask-reminder "feed the dogs" nil))
           ("make a reminder for me to do my taxes" ,(ask-reminder "do my taxes" nil))
           ("remind me to call the vet tomorrow" ,(ask-reminder "call the vet" 1))
           ("remind me tomorrow to call my mother" ,(ask-reminder "call my mother" 1))
           ("remind me friday to call my mother" ,(ask-reminder "call my mother" 2))
           ("remind me to pay taxes on monday" ,(ask-reminder "pay taxes" 5))
           ("remind me to call mum on friday at 9"
            ,(set-reminder "call mum" (- (* 2 86400) (* 5 3600))))
           ("set a reminder" ,(ask-reminder nil nil))
           ("can you set me a reminder" ,(ask-reminder nil nil))
           ("remind me" ,(ask-reminder nil nil))
           ("i need to set a reminder to go to the post office"
            ,(ask-reminder "go to the post office" nil))
           ("i want to get reminded to clean my room" ,(ask-reminder "clean my room" nil))
           ("remind me to call mum at 5 pm" ,(set-reminder "call mum" (* 3 3600)))
           ("remind me to call mum tomorrow at 9" ,(set-reminder "call mum" (* 19 3600)))
           ("set a reminder to call mum in 10 minutes" ,(set-reminder "call mum" 600)))
         do (is equal want (parsed text) "~s" text))
   ;; Alarms stay alarms.
   (true (uiop:string-prefix-p "Timer" (parsed "remind me at 5 pm")))))

(define-test reminders-questions-fill-the-gaps
  :parent va-tests
  (at-two-pm
   (loop for (text seconds) in `(("in 20 minutes" 1200) ("20 minutes" 1200)
                                 ("at 5 pm" ,(* 3 3600))
                                 ("tomorrow at 9" ,(* 19 3600))
                                 ("friday at 9" ,(- (* 2 86400) (* 5 3600))))
         do (is equal (set-reminder "call mum" seconds)
                (answered (va::ask-reminder "call mum" nil) text) "~s" text))
   (is equal (set-reminder "call mum" (* 19 3600))
       (answered (va::ask-reminder "call mum" 1) "9 am"))
   (is equal (ask-reminder "water the plants" nil)
       (answered (va::ask-reminder nil nil) "to water the plants"))
   (is equal "Dismiss" (answered (va::ask-reminder nil nil) "never mind"))
   (is equal "Dismiss" (answered (va::ask-reminder "call mum" nil) "forget it"))))

(define-test reminders-phrasings
  :parent va-tests
  (with-focus ()
    (is equal (set-reminder "call mum" 600) (parsed "Remind me to call mum in 10 minutes."))
    (is equal (set-reminder "check the oven" 3600)
        (parsed "Remind me in an hour to check the oven"))
    (parses-as "Reminder(List)"
               '(("What reminders do I have?") ("Tell me what's on my reminder list")
                 ("Read my reminders to me") ("What did I want to remember again?")
                 ("Do I have any reminders?")))))

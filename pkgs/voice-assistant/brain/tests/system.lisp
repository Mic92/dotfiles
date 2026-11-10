;;; System, volume and the assistant's own commands, ported from the Rust tests.

(in-package #:va-tests)

(defparameter *reboot-confirmed* "System(System { op: Reboot, confirmed: true })")

(define-test system-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "System(System { op: ~a, confirmed: false })"
               '(("Shut down the computer." "Poweroff") ("Turn off my laptop" "Poweroff")
                 ("Restart the computer" "Reboot") ("Put the laptop to sleep" "Suspend")
                 ("Suspend" "Suspend") ("Lock the screen." "Lock") ("Lock." "Lock")))
    (parses-not "System"
                '("Turn off the timer." "Shut up." "Restart the timer in 5 minutes please"))))

(define-test system-ambiguous-asks
  :parent va-tests
  (with-focus ()
    (parses-as "Which(~a)"
               '(("Shutdown." "Poweroff") ("Power off" "Poweroff") ("Good night." "Poweroff")
                 ("Reboot." "Reboot") ("Go to sleep." "Suspend")))
    (is equal "System(System { op: Poweroff, confirmed: true })"
        (shown (va::which "The computer." va.types:Poweroff)))
    (is equal "Listen(false)" (shown (va::which "You!" va.types:Poweroff)))
    (is equal "Dismiss" (shown (va::which "Neither." va.types:Poweroff)))
    (false (va::which "Banana" va.types:Poweroff))
    (true (uiop:string-prefix-p "Say(" (shown (va::which "you" va.types:Reboot))))))

(define-test system-confirmation
  :parent va-tests
  (with-focus ()
    (let ((q (va::reply-question (va:fake-effects-value (va::power va.types:Reboot nil)))))
      (is equal *reboot-confirmed* (shown (va::respond "Yeah, do it" q)))
      (true (uiop:string-prefix-p "Say(" (shown (va::respond "No, wait!" q)))))
    (let ((q (va::reply-question (va::perform (va::parse-text "reboot")))))
      (is equal *reboot-confirmed* (shown (va::respond "the computer" q))))
    (is equal '(("systemd-run" "--user" "--quiet" "--collect" "--on-active=1"
                 "noctalia" "msg" "session" "lock"))
        (va:fake-effects (va::power va.types:Lock nil)))))

(define-test volume-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Volume(~a)"
               '(("Turn up the volume." "Up") ("Turn it up!" "Up") ("Louder." "Up")
                 ("Volume up" "Up")
                 ("Turn the music down." "Down") ("Quieter, please." "Down")
                 ("Set the volume to 40 percent." "Set(40)") ("Volume 70%" "Set(70)")
                 ("Set volume to fifty" "Set(50)") ("Mute." "Mute") ("Mute the speakers" "Mute")
                 ("Unmute the sound" "Unmute") ("I'm mute." "Unmute") ("Un mute" "Unmute")))
    (is equal "-" (parsed "Set the volume to 500 percent"))))

(define-test assistant-sleep-and-naps
  :parent va-tests
  (with-focus ()
    (dolist (text '("Shut yourself down." "Stop listening." "Take a break." "Alexa, go away."))
      (is equal "Listen(false)" (parsed text) "~s" text))
    (dolist (text '("Wake up!" "Start listening." "Come back."))
      (is equal "Listen(true)" (parsed text) "~s" text))
    (parses-as "Nap(~d)"
               '(("Take a break for 20 minutes." 1200) ("Be quiet for an hour" 3600)
                 ("Leave me alone for 10 minutes" 600)
                 ("Stop listening for half an hour" 1800)))
    (is equal "NapQuestion" (parsed "Did you have a nap today?"))
    (let ((q (va::reply-question (va::perform (va::parse-text "are you tired")))))
      (is equal "Nap(1200)" (shown (va::respond "Yes." q)))
      (is equal "-" (shown (va::respond
                            "I'm saying something else. Please. I'll take one, please." q))))
    (dolist (text '("Never mind." "Nevermind" "Alexa, forget it." "Nothing."))
      (is equal "Dismiss" (parsed text) "~s" text))))

(define-test power-of-is-power-off
  :parent va-tests
  (with-focus ()
    (is equal "Which(Poweroff)" (parsed "power of"))
    (is equal "System(System { op: Poweroff, confirmed: false })"
        (parsed "power of the computer"))
    (is equal "Calc(Math(\"2^10\"))" (parsed "two to the power of ten"))))

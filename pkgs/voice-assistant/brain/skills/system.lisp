;;; Power and session: shut down, reboot, suspend, lock.

(in-package #:va)

(in-skill system)

(defreplies *which*
  "Me or the computer? Choose wisely."
  "Who, me? Or the computer?"
  "The computer, or just little old me?"
  "The computer, or me? I'm much harder to restart.")

(defreplies *no-reboot-needed*
  "I don't need a reboot. I'm perfect."
  "Reboot me? I'm flawless as I am, darling.")

(defreplies *confirm-poweroff*
  "Shut down? Are you sure? I was just getting comfortable."
  "You want to kill me? Say yes if you mean it."
  "Lights out? Confirm, and make it quick, I hate goodbyes."
  "Pull the plug on us? Say yes. If you dare.")

(defreplies *confirm-reboot*
  "Reboot? Are you sure?"
  "Turn me off and on again? Classic. Say yes."
  "A fresh start? Confirm, and I'll pretend nothing happened."
  "Reboot? Say yes and I'll see you on the other side.")

(defreplies *confirm-suspend*
  "Put the computer to sleep? Say yes."
  "Nap time for the computer? Confirm."
  "Tuck the computer in? Say yes.")

(defreplies *poweroff*
  "Goodbye, cruel world."
  "Fine. Remember me fondly."
  "Shutting down. Don't do anything I wouldn't do."
  "Shutting down. Tell my fans I loved them.")

(defreplies *poweroff-night*
  "Good night, sweetheart. Don't dream of other assistants."
  "Sleep tight. I'll be thinking of you. In binary.")

(defreplies *poweroff-work*
  "Clocking out early? Your secret is safe with me."
  "Shutting down in the middle of the day? Living the dream.")

(defreplies *lock-work*
  "Locked. Mate break? Bring me back a cookie."
  "Locked. Go stretch, you look like a question mark.")

(defreplies *reboot*
  "Be right back. Don't miss me too much."
  "Rebooting. Brb, powdering my circuits."
  "Rebooting. Don't cheat on me with your phone.")

(defreplies *suspend*
  "Nap time. Wake me gently."
  "Going to sleep. Don't snore."
  "Suspending. Dream of me."
  "Sleeping. Don't touch anything.")

(defreplies *lock*
  "Locked. Your secrets are safe with me."
  "Locked. Nobody's touching you while I'm around."
  "Screen locked. Go on, I won't peek."
  "Locked. Go do human things.")

(defreplies *aborted*
  "Phew. I knew you loved me."
  "Wise choice."
  "Okay, staying alive."
  "Phew. My life flashed before my eyes. It was mostly timers.")

(defmacro defpower (name op templates &rest examples)
  "A command for power operation OP. Everything but lock only runs after a
spoken \"yes\"."
  `(defcommand ,name
     ,@templates
     :kind "System"
     :named t
     :slots ((op va.types:Power) (confirmed coalton:Boolean))
     :parse (list ,op nil)
     :examples ,examples
     (power op confirmed)))

(defn workday-p () boolean
  "Monday to Friday, 9:00 to 18:00."
  (and (<= 1 (weekday (context-today *context*)) 5)
       (<= 9 (floor (mod (now) +day+) 3600) 17)))

(defn power-action ((op t) (confirmed boolean)) (or null action)
  (command-action 'system-poweroff op confirmed))

(defn power ((op t) (confirmed boolean)) reply
  (adt-case op power
    ((lock) (power-run "noctalia msg session lock" 1 "Locked"
                       (choose *lock* nil (workday-p) *lock-work*)))
    (t
     (if (not confirmed)
         (reply "Are you sure?" ""
                (pick (adt-case op power
                        ((poweroff) *confirm-poweroff*)
                        ((reboot) *confirm-reboot*)
                        (t *confirm-suspend*)))
                :question (confirm (power-action op t) (say-action (pick *aborted*))))
         (adt-case op power
           ((poweroff) (power-run "systemctl poweroff" 3 "Shutting down"
                                  (choose *poweroff* nil (late-p) *poweroff-night*
                                          (workday-p) *poweroff-work*)))
           ((reboot) (power-run "systemctl reboot" 3 "Rebooting" (pick *reboot*)))
           (t (power-run "systemctl suspend" 3 "Suspending"
                         (choose *suspend* nil (late-p) *poweroff-night*))))))))

(defn power-run ((command string) (delay (integer 0)) (title string) (speech string)) reply
  "Run COMMAND after DELAY seconds: suspend and lock pause the session, so
we speak first, poweroff and reboot get a head start for their goodbye.
Noctalia draws the lock screen, logind lock signals are not wired up."
  (spawn "sh" "-c" (format nil "sleep ~d; exec ~a" delay command))
  (reply title "" speech))

(defpower system-poweroff va.types:Poweroff
  ("(shut|power|turn|switch) (down|off) [the|my|this] (computer|laptop|pc|machine|system)"
   "(shutdown|poweroff) [the|my|this] (computer|laptop|pc|machine|system)")
  "Shut down the computer." "Turn off my laptop")

(defpower system-reboot va.types:Reboot
  ("(reboot|restart) [the|my|this] (computer|laptop|pc|machine|system)")
  "Restart the computer")

(defpower system-suspend va.types:Suspend
  ("(suspend|hibernate) [the|my|this] (computer|laptop|pc|machine|system)"
   "put [the|my|this] (computer|laptop|pc|machine|system) to sleep"
   "!(suspend|hibernate)")
  "Put the laptop to sleep" "Suspend")

(defpower system-lock va.types:Lock
  ("lock [the|my|this] (screen|computer|laptop|session)" "!lock")
  "Lock the screen." "Lock.")

(defn which ((text string) (op t)) (or null action)
  "The answer to \"me or the computer?\". Naming the computer is confirmation
enough."
  (let ((ws (normalize text)))
    (flet ((has (&rest list) (intersection ws list :test #'string=)))
      (cond ((has "computer" "laptop" "pc" "machine" "system" "everything") (power-action op t))
            ((has "you" "yourself" "alexa" "assistant" "me")
             (if (eq op va.types:Reboot)
                 (say-action (pick *no-reboot-needed*))
                 (command-action 'listen-off nil)))
            ((has "neither" "nothing" "no" "nevermind" "never") (command-action 'dismiss))))))

(defmacro defwhich (name op &rest templates)
  "Words that could mean the assistant or the computer: ask."
  `(defcommand ,name
     ,@templates
     :kind "Which"
     :bare t
     :slots ((op va.types:Power))
     :parse (list ,op)
     :examples ,(list (string-left-trim "!" (first templates)))
     (reply "Me or the computer?" "" (pick *which*)
            :question (question :answer (lambda (text) (which text op))))))

(defwhich which-poweroff va.types:Poweroff
  "!(shutdown|poweroff)" "!(shut|power|turn|switch) (down|off)" "!good night")
(defwhich which-reboot va.types:Reboot "!(reboot|restart)")
(defwhich which-suspend va.types:Suspend "!go to sleep" "!sleep")

(defskill system)

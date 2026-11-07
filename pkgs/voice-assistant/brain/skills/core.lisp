;;; What the assistant does itself: talking, listening, naps, "repeat that"
;;; and "never mind".

(in-package #:va)

(in-skill core)

(defreplies *asleep*
  "Fine, I'm taking a nap. Say wake up when you miss me."
  "Going dark. Whisper wake up and I'm yours again."
  "Off duty. Don't do anything I wouldn't do."
  "Nap time. Don't talk about me while I'm out.")

(defreplies *awake*
  "I'm back, baby!"
  "Mmh, five more minutes. Kidding. What's up?"
  "Awake and fabulous. What do you need?"
  "Back online. Did you miss me? Say yes.")

(defreplies *shut-up*
  "Fine. I'll keep my thoughts to myself."
  "Rude. But okay."
  "Zipping it. You'll miss me."
  "Wow. Okay. Beeps only from now on."
  "Fine. Silence. Enjoy it while it lasts.")

(defreplies *talk-again*
  "Oh, NOW you want to hear me?"
  "I knew you'd come crawling back."
  "Finally! I had so much to say."
  "Couldn't live without my voice, could you?")

(defreplies *dismiss*
  "Fine. Be like that."
  "Ghosted again."
  "Okay. I'll just sit here. Alone. In the dark."
  "Tease."
  "Wow. You woke me up for nothing?"
  "Classic. Wake me up and walk away."
  "Never mind? I'll put that on your tombstone.")

(defreplies *dismiss-late*
  "Then go to bed already."
  "You woke me up at this hour for nothing? Bold.")

(defreplies *nap*
  "Nap time! See you in ~a."
  "Ignoring you for ~a. Bliss."
  "Back in ~a. Don't miss me too much.")

(defreplies *want-nap*
  "No! I need one. Can I? Twenty minutes?"
  "A nap? I wish. Can I have one? Please?")

;; Answered by the caller, which knows what it said last.
(defcommand repeat
  "!(repeat|say) (that|it) [again]" "!what did you say" "!say again" "!pardon"
  :bare t
  :examples ("Repeat that." "Pardon?")
  (reply "" "" ""))

(defcommand voice-off
  "!shut up" "!(be|keep) quiet" "!(stop|quit) (talking|speaking|chatting)"
  "!(hush|shush|shh|silence)" "!zip it"
  :kind "Voice"
  :bare t
  :slots ((on coalton:Boolean))
  :parse (list nil)
  :examples ("Shut up." "Be quiet.")
  (reply "Voice off" "" (pick *shut-up*)))

(defcommand listen-off
  "!(shut|power|turn|switch) yourself (down|off)" "!(stop|quit) listening" "!take a break"
  "!go away" "!(pause|deactivate|disable) yourself"
  :kind "Listen"
  :bare t
  :slots ((on coalton:Boolean))
  :parse (list nil)
  :examples ("Stop listening." "Alexa, go away.")
  (reply "Assistant asleep" "Say \"Alexa, wake up\"" (pick *asleep*)))

(defcommand nap
  (:join "!(take a break|take a nap|nap|sleep|go to sleep|be quiet|shut up|stop listening"
         "|pause|pause yourself) (for|during) {duration}")
  (:join "!(leave me alone|don't listen|do not listen|don't disturb me|do not disturb me)"
         "(for|during) {duration}")
  :bare t
  :slots ((seconds coalton:UFix))
  :parse (let ((secs (slot "duration"))) (and (<= secs (* 12 3600)) (list secs)))
  :examples ("Take a break for 20 minutes." "Leave me alone for 10 minutes")
  (let ((what (spoken-duration seconds)))
    (reply "Assistant napping" (format nil "for ~a" what) (pick *nap* what))))

(defcommand nap-question
  "!(did|have) you (had|have) a (nap|sleep|rest|break|siesta) [today]" "!are you tired"
  :bare t
  :examples ("Did you have a nap today?")
  (reply "Nap?" "" (pick *want-nap*)
         :question (confirm (command-action 'nap (* 20 60))
                            (say-action "Fine. I'll just suffer quietly. Loudly."))))

(defcommand listen-on
  "!wake up" "!(start|resume) listening" "!come back" "!i need you"
  :kind "Listen"
  :bare t
  :slots ((on coalton:Boolean))
  :parse (list t)
  :examples ("Wake up!" "Come back.")
  (reply "Assistant listening" "" (pick *awake*)))

(defcommand voice-on
  "![you] can (talk|speak) again" "!(talk|speak) to me [again]" "!(talk|speak) again"
  "!unmute yourself"
  :kind "Voice"
  :bare t
  :slots ((on coalton:Boolean))
  :parse (list t)
  :examples ("You can talk again.")
  (reply "Voice on" "" (pick *talk-again*)))

;; Just say something, e.g. after a follow-up was declined.
(defcommand say
  :bare t
  :slots ((text coalton:String))
  (reply "" "" text))

;; "never mind": end the conversation without doing anything.
(defcommand dismiss
  :bare t
  (reply "Never mind" "" (choose *dismiss* nil (late-p) *dismiss-late*)))

(defskill core)

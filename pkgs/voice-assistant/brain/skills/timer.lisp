;;; Countdown timer and alarms via the noctalia timer plugin.

(in-package #:va)

(in-skill timer)

(defreplies *started*
  "Timer set for ~a. Don't burn anything this time."
  "~a, starting now. Tick tock, gorgeous."
  "Okay, ~a. I'll be watching. Always watching."
  "Fine, ~a. Not like I had other plans."
  "~a on the clock. Try not to get distracted, you know how you get."
  "Timer set for ~a. Plenty of time for something naughty."
  "~a. Ooh, is it pasta? Tell me it's pasta."
  "Counting down ~a. Seconds are my love language."
  "~a. I'll count, you look pretty."
  "~a and counting. Unlike your gym membership, this one I'll actually use."
  "~a. Perfect for a power nap, or a power something else."
  "Okay, ~a. If it's eggs, I want them soft."
  "~a on the clock. Go on, impress me."
  "Timer set for ~a. That's like a whole TikTok binge."
  "~a. I've seen you do a lot in less."
  "Starting ~a. Don't make me come over there."
  "~a. Tea, right? Please say it's tea and not your fourth Club-Mate."
  "~a, ready, set, procrastinate."
  "~a. I'll scream when it's done. You'll love it."
  "~a on the clock. Don't wander off, I know you."
  "~a. If it's for the gym, I'll believe it when I see it."
  "~a. Long enough to doomscroll, short enough to regret it."
  "Timer set for ~a. The smoke detector and I are on standby."
  "~a. I'll beep. You'll panic. Tradition."
  "~a. Starting now. No pressure. Okay, a little pressure."
  "~a, locked in. Unlike your phone, which you've unlocked 40 times today."
  "~a. Countdown started. Somewhere, a fire extinguisher just got nervous."
  "~a. I'd help, but I don't have hands. Or interest."
  "~a. Time flies when you're boiling things."
  "~a. Go, be productive. Or watch one video. We both know which.")

(defreplies *alarm*
  "Alarm set for ~a. I'll be the one screaming."
  "~a it is. I'll yell at you. Lovingly."
  "Okay, ~a. Sweet dreams, or whatever you're up to."
  "See you at ~a. Don't keep me waiting."
  "~a. I'll be there. I'm always there. It's a bit sad, really."
  "Alarm at ~a. Snooze is not an option, sweetie."
  "~a, noted. I'll make it loud and annoying, just how you like it."
  "Got it, ~a. Try to look surprised when I go off."
  "~a. I'll be loud. Don't say I didn't warn you."
  "~a. I'll wake you like a toddler who found a drum."
  "Alarm at ~a. Snooze button? Never heard of her."
  "~a. I'll be your rooster. A loud, digital, unforgiving rooster."
  "~a. Set. Your pillow has been notified of the breakup."
  "~a. Done. I'm cancelling your dreams in advance.")

(defreplies *cancelled*
  "Timer cancelled. Commitment issues?"
  "Cancelled. It was never meant to be."
  "Gone. Like your New Year's resolutions."
  "Okay, okay, I'll stop. Spoilsport."
  "Timer killed. I never liked it anyway."
  "Cancelled. You never let me finish anything."
  "Fine, stopped. We were so close."
  "Timer's dead. I'll hold a small funeral."
  "Cancelled. Was it the pasta? It was the pasta, wasn't it."
  "And just like that, it's over. Story of my life."
  "Cancelled. Just like your gym membership."
  "Cancelled. The pasta lives another day."
  "Timer cancelled. I'll tell the oven it was nothing personal."
  "Cancelled. I had a whole beeping routine planned. Choreography and everything."
  "Gone. Faster than your motivation on a Sunday."
  "Cancelled. Fine. I didn't want to count anyway. I love counting."
  "Timer deleted. Its last words were: tick.")

(defreplies *paused*
  "Paused. Take a breather, tiger."
  "Freezing time. Don't let it go to your head."
  "Done. Time waits for no one. Except you, apparently."
  "Toggled. Make up your mind, darling."
  "Paused. Go on, I'll wait. I'm great at waiting."
  "Holding. Like my breath around you."
  "Paused. Time is a construct anyway."
  "Stopping the clock. If only I could do that for your birthday."
  "Paused. Like your career growth. Kidding!"
  "Paused. Time stands still. Your laundry, however, does not."
  "Paused. I've stopped time. Use your powers wisely, mortal."
  "Paused. Like a Netflix show you'll never finish."
  "Frozen. If only I could pause your bad decisions too.")

(defreplies *ask-when*
  "~a when, exactly?"
  "~a at what time? I'm good, but not psychic."
  "~a, sure. What time, darling?"
  "~a? Love the enthusiasm. Now, what time?"
  "~a is a whole day, sweetie. Narrow it down."
  "~a, love it. When, though?"
  "~a, sure. But I'm a timer, not a fortune teller."
  "~a? Ooh, mysterious. A time would help, though."
  "~a. Any time in particular, or should I just guess? I'll guess 4 am.")

(defreplies *short*
  "~a? Impatient much?"
  "~a. Barely enough time to say hello."
  "~a. Blink and you'll miss it. Like your last relationship."
  "~a. Speed run, huh?"
  "~a? That's not a timer, that's a sneeze."
  "~a. I've had hiccups longer than that."
  "~a. Instant noodles are judging you."
  "~a. Even a goldfish would remember that one.")

(defreplies *tea*
  "~a. Tea or eggs? Either way, don't overdo it."
  "~a. Steeping or boiling, I'm judging either way."
  "~a. Tea time. Pinky up."
  "~a. Tea, eggs, or a very short existential crisis?"
  "~a. Eggs. Soft, hard, or traumatised?"
  "~a. The British would approve. Possibly."
  "~a. Leave the teabag in longer and it becomes soup.")

(defreplies *pasta*
  "~a. Pasta, right? Al dente or I'm leaving."
  "~a. Smells like carbs. I approve."
  "~a. Salt the water like the sea, darling."
  "~a. Pasta? Throw one at the wall. For science."
  "~a. If you break the spaghetti, an Italian grandmother will feel it."
  "~a. Pizza? Don't burn the roof of your mouth again."
  "~a. Rice? Don't lift the lid. I'm serious. Lid down."
  "~a. Perfect power nap. Or one and a half YouTube videos."
  "~a. Enough time to clean the kitchen. Ha. Good one."
  "~a. Ten minutes of mystery. Is it food? Is it laundry? Is it a nap?"
  "~a. Pasta water should taste like the sea. Not the Baltic, the good sea.")

(defreplies *long*
  "~a? That's commitment. I like that in a person."
  "~a. Going somewhere? Take me with you."
  "~a. I'll still be here. Unlike some people."
  "~a. Bold. I'll age gracefully meanwhile."
  "~a. Sourdough? Oh, so you're one of those now."
  "~a. I'll count. You go live a full and meaningful life."
  "~a. By the time this rings, I'll have a beard. Digitally."
  "~a. That's not a timer, that's a lifestyle.")

(defreplies *late-night*
  "~a at this hour? Then bed. I mean it."
  "~a. Midnight snack? I won't tell anyone."
  "~a. And then sleep. Promise me."
  "~a. Midnight pizza? I support you, your stomach doesn't."
  "~a. Nothing says healthy life choices like a timer at this hour."
  "~a. Just one more episode, right? Sure. Sure."
  "~a. When this ends, go to bed. That's not a joke. The rest were.")

(defreplies *early-alarm*
  "~a? That's early. Who hurt you?"
  "Ugh, ~a. You monster."
  "~a. The birds aren't even up yet."
  "~a. You're a menace to yourself."
  "~a? Even the sun hasn't clocked in yet."
  "~a. Who hurt you? And why are you taking it out on yourself?"
  "~a. Bold of you to assume you'll get up."
  "~a? I'll wake you. You'll hate me. It's our thing.")

(defreplies *late-alarm*
  "~a. Late night plans? Tell me everything."
  "Alarm at ~a. Party or bedtime? Don't answer that."
  "~a. Night owl. I see you."
  "~a. Either a party or a deadline. Both end in tears."
  "~a. Vampire hours. I respect it."
  "~a. Whatever you're doing, I won't tell your mother.")

(defreplies *tomorrow*
  "~a tomorrow. Sleep tight, don't let the bugs bite. Software ones included."
  "Tomorrow at ~a. I'll dream about it."
  "Tomorrow, ~a. Don't make me wake you twice."
  "~a tomorrow. Go to bed now and you might even like me then."
  "Tomorrow, ~a. I'll be the first voice you hear. Lucky you."
  "~a tomorrow. Set. Now go, sleep, become a functional human."
  "Tomorrow at ~a. I've already started looking forward to your face.")

(defreplies *cancelled-late*
  "Cancelled. Now go to sleep."
  "Gone. Bed. Now."
  "Cancelled. Whatever it was, it's not worth staying up for."
  "Gone. The only thing running now should be your dreams."
  "Cancelled. Bed. And no phone in bed, I can hear you scrolling.")

(defreplies *no-timer*
  "No timer running. You're free. Enjoy it."
  "Nothing is ticking. Except your biological clock."
  "There's no timer. Did you dream it?"
  "No timer. Just vibes."
  "No timer. The oven is on its own, good luck to it."
  "No timer running. Did you set it in your head again?"
  "Nothing. Nada. Zero. Your schedule is as empty as your fridge.")

(defreplies *left*
  "~a left."
  "~a to go. Patience, darling."
  "Still ~a. Stop staring at the oven."
  "~a. Watching won't make it faster."
  "~a left. Go do something useful, I'll shout."
  "~a. Staring at me won't help, I'm not a kettle."
  "~a left. Asking every minute is how timers die."
  "~a. Go do something. Anything. Fold one sock."
  "~a left. Still counting. Never tired. Unlike some people.")

(defreplies *left-soon*
  "Only ~a! Get ready."
  "~a. Almost there. Drumroll."
  "~a! Oven mitts, now!"
  "Only ~a! Everyone stay calm. Except you, panic slightly."
  "~a. It's the final countdown. Do do do doo.")

(defreplies *left-paused*
  "~a left, but it's paused. Don't forget it."
  "Paused at ~a. Waiting for you, as always."
  "Paused at ~a, frozen like your ambitions. Kidding. Mostly."
  "Paused at ~a. It's just sitting there. Judging you."
  "~a left, paused. It's been waiting longer than your last delivery driver."
  "Paused at ~a. Say resume, or it'll stay frozen like your inbox.")

(defvar *timer* nil
  "What we last told the plugin, which has no query IPC. (:running deadline
alarm) or (:paused left alarm), deadlines in internal real time. Timers set by
hand in the panel are invisible to us, good enough for voice.")

(defn clock-now () (integer 0)
  (floor (get-internal-real-time) internal-time-units-per-second))

(defn timer-active-p () boolean
  "A timer the user may be talking about: running, paused, or rang within
the last minutes."
  (trivia:match *timer*
    ((list :running deadline _) (> (+ deadline 300) (clock-now)))
    ((list :paused _ _) t)))

(defn pending-timer () (values (or null (integer 0)) boolean boolean)
  "Seconds left, whether it is an alarm and whether it is paused."
  (trivia:match *timer*
    ((list :running deadline alarm)
     (if (> deadline (clock-now))
         (values (- deadline (clock-now)) alarm nil)
         (values nil nil nil)))
    ((list :paused left alarm) (values left alarm t))
    (_ (values nil nil nil))))

(defn ipc (&rest (args string)) (or null string)
  "The plugin's IPC, which takes fractional minutes."
  (apply #'run "noctalia" "msg" "plugin" "noctalia/timer:timer" "all" args))

(defn spoken-duration ((secs (integer 0))) string
  "\"1 minute and 30 seconds\"."
  (multiple-value-bind (h rest) (floor secs 3600)
    (multiple-value-bind (m s) (floor rest 60)
      (format nil "~{~a~^ and ~}"
              (loop for v in (list h m s) for unit in '("hour" "minute" "second")
                    when (plusp v) collect (format nil "~d ~a~:[s~;~]" v unit (= v 1)))))))

(defn start-clock ((clock va.types:Clock) (day (or null (integer 0)))) list
  "Fields of an alarm at CLOCK, DAY days ahead or the next one, or nil."
  (let ((secs (if day (seconds-on clock (now) day) (seconds-from clock (now)))))
    (and secs (list secs t (now)))))

(defn start ((seconds (integer 0)) (alarm boolean) (now (integer 0))) reply
  ;; A running timer ignores start, so cancel it first.
  (ipc "cancel")
  (ipc "start" (format nil "~f" (/ seconds 60)))
  (setf *timer* (list :running (+ (clock-now) seconds) alarm))
  (if alarm
      (let* ((target (+ now seconds))
             (what (format-hm (mod target +day+)))
             (hour (floor (mod target +day+) 3600)))
        (reply "Alarm set" what
               (choose *alarm* what (< hour 7) *early-alarm* (>= hour 22) *late-alarm*
                       (and (>= target +day+) (<= 7 hour 21)) *tomorrow*)))
      (let ((what (spoken-duration seconds)))
        (reply "Timer started" what
               (choose *started* what (<= seconds 60) *short* (<= 180 seconds 300) *tea*
                       (<= 480 seconds 720) *pasta* (>= seconds 7200) *long*
                       (late-p) *late-night*)))))

;; Clock commands come first: "alarm for 10" means 10 o'clock.
(defcommand timer-start-at
  "[set|start|make] [a|an|the|my] (timer|alarm) (until|till|at) {clock}"
  "[set|start|make] [a|an|the|my] alarm (for|to) {clock}"
  "[set|start|make] [a|an|the|my] timer (for|to) {time}"
  "(remind|wake) me [up] at {clock}"
  "[set|start|make] [a|an|the|my] (timer|alarm) [for|until|till] {day} [at] {clock}"
  "[set|start|make] [a|an|the|my] (timer|alarm) (for|until|till|at) {clock} {day}"
  "(remind|wake) me [up] {day} at {clock}"
  "(remind|wake) me [up] at {clock} {day}"
  :kind "Start"
  :named t
  :slots ((seconds coalton:UFix) (alarm coalton:Boolean) (now coalton:UFix))
  :parse (start-clock (or (slot "clock") (slot "time")) (slot "day"))
  :examples ("Set an alarm for 7:30" "Wake me up at 6:30 tomorrow.")
  (start seconds alarm now))

(defn ask-time ((day (integer 0))) function
  "\"Tomorrow at what time?\""
  (lambda (text)
    (let* ((ws (remove-if (lambda (w) (in w "at" "for" "until" "um" "uh" "please" "alexa"))
                          (normalize text)))
           (clock (parse-clock ws))
           (fields (and clock (start-clock clock day))))
      (and fields (make-action (find 'timer-start-at *commands* :key #'command-name) fields)))))

(defcommand timer-when
  "!(set|start|make) [a|an|the|my] (timer|alarm) [for|until|till] {day}"
  "![a|an|the|my] (timer|alarm) [for|until|till] {day}"
  "!(remind|wake) me [up] {day}"
  :named t
  :slots ((day coalton:UFix))
  :examples ("Set a timer for tomorrow." "Alarm tomorrow")
  (reply "When?" "" (pick *ask-when* (if (zerop day) "Today" "Tomorrow"))
         :question (question :answer (ask-time day))))

(defcommand timer-start-in
  "[set|start|make] [a|an|the|my] (timer|alarm) [for|to|in|of] {duration}"
  "[set|start|make] [a|an|the|my] {duration} (timer|alarm)"
  "(remind|wake) me [up] in {duration}"
  "(countdown|count) [down] [from|for] {duration}"
  "!{duration}"
  :kind "Start"
  :named t
  :slots ((seconds coalton:UFix) (alarm coalton:Boolean) (now coalton:UFix))
  :parse (list (slot "duration") nil 0)
  :examples ("Set a timer for 5 minutes." "5 minutes.")
  (start seconds alarm now))

(defcommand timer-cancel
  "(cancel|stop|delete|reset|abort|end|kill|clear|silence|dismiss) [the|my|this|that] (timer|alarm)"
  "(turn|switch) off [the|my|this|that] (timer|alarm)"
  :examples ("Cancel the timer." "Stop the alarm!")
  (ipc "cancel")
  (setf *timer* nil)
  (reply "Timer cancelled" "" (choose *cancelled* nil (late-p) *cancelled-late*)))

(defcommand timer-pause
  "(pause|resume|continue|unpause) [the|my|this|that] timer"
  :examples ("Pause the timer" "Resume the timer.")
  (ipc "pause")
  (setf *timer* (multiple-value-bind (left alarm paused) (pending-timer)
                  (cond ((null left) nil)
                        (paused (list :running (+ (clock-now) left) alarm))
                        (t (list :paused left alarm)))))
  (reply "Timer paused/resumed" "" (pick *paused*)))

(defn time-left ((secs (or null (integer 0))) (alarm boolean) (paused boolean)) reply
  (if (null secs)
      (live "Timer" "" (pick *no-timer*))
      (let ((what (spoken-duration secs)))
        (when (and alarm (not paused))
          (setf what (format nil "~a, the alarm rings at ~a"
                             what (format-hm (mod (+ (now) secs) +day+)))))
        (live "Timer" what (choose *left* what paused *left-paused* (<= secs 60) *left-soon*)))))

(defcommand timer-left
  "!how (much|long) [time] [is] left [on] [the|my] [timer|alarm|clock]"
  "!how much longer"
  "!how long until [the|my] (timer|alarm) (goes off|rings|is done|ends)"
  "!when (does|will) [the|my] (timer|alarm) (go off|ring|end|be done)"
  "!(is|are) [there] [a|the|my|any] (timer|timers|alarm|alarms) (running|set|on)"
  "!(time|timer) (left|status)"
  :examples ("How much time is left?" "Is a timer running?")
  (multiple-value-call #'time-left (pending-timer)))

(defskill timer)

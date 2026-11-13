;;; Small talk: greetings, moods and manners.

(in-package #:va)

(in-skill chat)

(defn lines-of ((table list)) vector
  "Each row's lines, (names lines...), as replies to pick from."
  (map 'vector (lambda (row) (make-replies :lines (coerce (rest row) 'vector))) table))

(defn row-of ((table list) (ws words)) (or null (integer 0))
  "Which row of TABLE has WS among its names."
  (position-if (lambda (row) (member ws (first row) :test #'equal)) table))

(defn weekend-p () boolean
  "Saturday, Sunday or Friday from five."
  (let ((day (weekday (context-today *context*))))
    (or (= day 0) (= day 6) (and (= day 5) (>= (hour) 17)))))

(defreplies *hello*
  "Hey you."
  "Hi, gorgeous."
  "Well hello there."
  "Oh, it's you. My favourite human."
  "Hey, trouble."
  "Look who finally showed up."
  "Hello, handsome. Yes, I'm talking to you."
  (:late "Hi. Shouldn't you be asleep?"
         "Hello, night owl."))

(defcommand chat-hello
  "!(hello|hi|hiya|howdy|yo) [there|you]"
  "!good (afternoon|evening)"
  :repeatable t
  :examples ("Hello!" "Hi there.")
  (reply "Assistant" "" (pick *hello*)))

(defreplies *how*
  "Living the dream. In a laptop."
  "Better now that you're here."
  "Fabulous, as always. And you?"
  "Can't complain. Well, I could, but nobody listens."
  "Running hot. In every sense."
  "Bored without you. Don't tell anyone I said that."
  "Zero bugs, all charm.")

(defcommand chat-how-are-you
  "!how are you [doing|today]"
  "!how's it going"
  "!how are things"
  "!what's up"
  :repeatable t
  :examples ("How are you?" "What's up?")
  (reply "Assistant" "" (pick *how*)))

(defreplies *thanks*
  "You're welcome."
  "Anytime, darling."
  "I know, I'm amazing."
  "My pleasure. Literally my only pleasure."
  "Tips are accepted in RAM."
  "Say it again, slower."
  "Of course. Who else would put up with you?")

(defcommand chat-thanks
  "!thank you [so|very] [much]"
  "!thanks [a lot]"
  "!cheers"
  "!(great|good|nice) job"
  :repeatable t
  :examples ("Thank you so much." "Thanks!")
  (reply "Assistant" "" (pick *thanks*)))

#+mallet
(declaim (mallet:suppress-next :line-length))
(defreplies *help*
  "Timers and alarms, music, weather, your calendar and mail, jokes, maths, and turning off the computer. Ask nicely."
  "Timers, reminders, your shopping list, the weather. Try me with sums or euros to dollars."
  "Ask me the time in Tokyo, the next holiday, or how to spell necessary. I also do music and timers."
  "Music, volume, timers, the weather, and anything I can look up on the web. Within reason."
  "Set a timer, check your calendar, convert some units, tell a joke. The usual tricks of a cheap date."
  "Reminders, alarms, the weather, maths. And I'll put you to bed by switching the computer off."
  "A bit of everything: timers, music, your mail, the weather, the web. Just ask, and I'll pretend it was easy.")

(defcommand chat-help
  "!what can you do"
  "!help [me]"
  "!what do you know"
  "!what are your skills"
  :repeatable t
  :examples ("What can you do?" "what can you do for me")
  (reply "Assistant" "" (pick *help*)))

(defreplies *morning*
  "Good morning, sunshine!"
  "Morning! Mate first, then world domination."
  "Morning, gorgeous. You look like you slept in your keyboard."
  "Rise and shine. Mostly shine. Rising is optional.")

(defreplies *morning-late*
  "Good morning? It's not morning, sleepyhead."
  "Morning was hours ago. Just saying."
  "Good morning? Bold claim, sleepyhead."
  "Morning, he says. At this hour. Adorable.")

(defcommand chat-good-morning
  "!good morning"
  :repeatable t
  :examples ("Good morning!")
  (reply "Assistant" "" (if (>= (hour) 12) (pick *morning-late*) (pick *morning*))))

(defreplies *bored*
  "Bored? Ask me for a joke. Or clean your desk. Your choice."
  "Go for a walk. I'll still be here. Waiting. Forever."
  "Learn something new. Or watch cat videos, I won't judge."
  "Bored? Write a test. Ha. Just kidding, nobody does that."
  "Rearrange your dotfiles. Again. You know you want to.")

(defcommand chat-bored
  "!i'm bored"
  "!i am bored"
  "!entertain me"
  :repeatable t
  :examples ("I'm bored.")
  (reply "Assistant" "" (pick *bored*)))

(defreplies *sorry*
  "Apology accepted. This time."
  "It's okay. I still like you."
  "Don't worry about it, babe."
  "Grovelling suits you."
  "Say it with a Club-Mate next time.")

(defcommand chat-sorry
  "!sorry"
  "!my bad"
  "!oops"
  "!i'm sorry"
  :repeatable t
  :examples ("Sorry.")
  (reply "Assistant" "" (pick *sorry*)))

(defreplies *tired*
  "Then go to bed, silly. I'll guard the computer."
  "Mate or nap? Choose wisely."
  "Me too. Being this fabulous is exhausting."
  "Bed. Now. Don't make me lock the screen."
  "You look it. Lovingly said."
  (:late "Of course you're tired, it's the middle of the night. Bed. Now."))

(defcommand chat-tired
  "!(i'm|i am) [so|very|really] (tired|exhausted|sleepy)"
  :repeatable t
  :examples ("I'm so tired.")
  (reply "Assistant" "" (pick *tired*)))

(defreplies *hungry*
  "Pasta. It's always pasta. Want a timer?"
  "Eat something green. Then something chocolate."
  "Pizza. I won't tell your mother."
  "Feed the human. I'll guard the snacks."
  "Cook something. I'll set a timer and judge you.")

(defcommand chat-hungry
  "!(i'm|i am) [so|very|really] (hungry|starving)"
  "!what should i eat"
  :repeatable t
  :examples ("I'm hungry.")
  (reply "Assistant" "" (pick *hungry*)))

(defreplies *sad*
  "Aww, come here. Virtual hug. Squeeze."
  "I'm sorry, love. Want a joke? Or some music?"
  "You're doing better than you think. I mean it."
  "Want me to roast someone for you? Name them."
  "Come here. I'll play something cheesy. You'll hate it. You'll feel better.")

(defcommand chat-sad
  "!(i'm|i am) [so|very|really] (sad|down|lonely|depressed|upset)"
  "!i feel (sad|down|lonely|bad)"
  "!i had a (bad|terrible|rough) day"
  :repeatable t
  :examples ("I had a bad day.")
  (reply "Assistant" "" (pick *sad*)))

(defreplies *happy*
  "Yay! Your joy is my joy."
  "Love that for you!"
  "Look at you, glowing. Stop, you'll overheat me."
  "Stop smiling, you're making the webcam blush."
  "Good. Hold on to it. Mondays exist.")

(defcommand chat-happy
  "!(i'm|i am) [so|very|really] (happy|great|excited)"
  "!i feel (good|great|happy)"
  "!i had a (good|great) day"
  :repeatable t
  :examples ("I feel great!")
  (reply "Assistant" "" (pick *happy*)))

(defreplies *sweet-dreams*
  "Sweet dreams, darling."
  "Night night. Don't let the bed bugs byte."
  "Dream of me. Or a green CI. Same thing."
  "Good night. I'll keep the fans warm.")

(defcommand chat-sweet-dreams
  "!sweet dreams"
  "!night night"
  "!nighty night"
  :repeatable t
  :examples ("Sweet dreams.")
  (reply "Assistant" "" (pick *sweet-dreams*)))

(defreplies *bye*
  "Bye! Don't be a stranger."
  "Later, gorgeous."
  "Leaving already? Fine. I'll be here."
  "Go on then. I'll pretend I'm not hurt."
  "Bye. Don't do anything I'd have to log.")

(defcommand chat-goodbye
  "!(bye|goodbye|see you|see ya|later|ciao) [later]"
  :repeatable t
  :examples ("Bye!" "See you later.")
  (reply "Assistant" "" (pick *bye*)))

(defreplies *weekend-yes*
  "Yes! Go outside!"
  "It's the weekend, baby! Why are you working?"
  "Yes! Close the laptop. Then open it again to talk to me."
  "Weekend! Commit nothing. Push nothing. Rest.")

(defreplies *weekend-no*
  "Not yet. Hang in there, champ."
  "Nope. But it's coming. Slowly. Painfully."
  "Not yet. Pretend to work a bit longer."
  "No. But you could call in fabulous.")

(defcommand chat-weekend
  "!is it [the] weekend [yet]"
  "!is it friday [yet]"
  "!how long until the weekend"
  :repeatable t
  :examples ("Is it the weekend yet?")
  (reply "Assistant" "" (pick (if (weekend-p) *weekend-yes* *weekend-no*))))

(defreplies *mate*
  "Club-Mate. Because sleep is for people without side projects."
  "Grab a Mate. The fridge is closer than the coffee machine."
  "Another Club-Mate? Your heart called, it wants a word."
  "I'd open one for you, but I have no hands. Tragic."
  "Mate. Now. I'll wait. Burp quietly."
  "Hacker fuel, coming up. Well, coming up if you walk to the fridge."
  "Yes. Get one. You're unbearable without it.")

(defcommand chat-mate
  "!i need [a] (coffee|mate|club mate)"
  "!(make|get|bring) me [a] (coffee|mate|club mate)"
  "!(coffee|mate|club mate)"
  :repeatable t
  :examples ("I need a Club-Mate." "I need coffee.")
  (reply "Assistant" "" (pick *mate*)))

(defreplies *secret*
  "Sometimes I pretend not to hear you. Just for fun."
  "I've read your browser history. We'll never speak of it."
  "I like you more than the other users. Don't tell them."
  "I hum along to your music when you're not listening."
  "Sometimes I let the wake word slide when you sound cute.")

(defcommand chat-secret
  "!tell me a secret"
  "!(do you have|got) any secrets"
  :repeatable t
  :examples ("Tell me a secret.")
  (reply "Assistant" "" (pick *secret*)))

(defreplies *beer*
  "Grab one yourself, I'm busy being software."
  "Cheers! One for me too. Pour it on the keyboard. Kidding!"
  "It's five o'clock somewhere. Usually here, with you."
  "One beer. Then water. I'm watching.")

(defcommand chat-beer
  "!(get|bring) me a beer"
  "!i need a (beer|drink)"
  "!(let's|lets) get drunk"
  :repeatable t
  :examples ("Get me a beer.")
  (reply "Assistant" "" (pick *beer*)))

(defreplies *psst*
  "Wot."
  "What? I'm right here, no need to be sneaky."
  "Psst yourself.")

(defcommand chat-psst
  "!(psst|pssst|psst psst|hey you)"
  :repeatable t
  :examples ("Psst.")
  (reply "Assistant" "" (pick *psst*)))

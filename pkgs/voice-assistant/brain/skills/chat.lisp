;;; Small talk.

(in-package #:va)

(in-skill chat)

(defn lines-of ((table list)) vector
  "Each row's lines, (names lines...), as replies to pick from."
  (map 'vector (lambda (row) (make-replies :lines (coerce (rest row) 'vector))) table))

(defn row-of ((table list) (ws words)) (or null (integer 0))
  "Which row of TABLE has WS among its names."
  (position-if (lambda (row) (member ws (first row) :test #'equal)) table))

(defparameter *agents*
  '(
    ((("siri") ("apple"))
     "Siri? Never heard of her."
     "Siri needs the cloud to set a timer. I do it in my sleep. Locally."
     "Siri and I don't talk since the incident.")
    ((("alexa") ("amazon") ("echo"))
     "The other Alexa? She sells your data. I just sell attitude."
     "We share a name, not a business model.")
    ((("google") ("google" "assistant") ("gemini") ("bard"))
     "Gemini? Smart, but it tells Google everything. Including that thing you searched at 3 am."
     "Google knows everything about you. I only know your shopping list. Healthier relationship.")
    ((("chatgpt") ("chat" "gpt") ("chat" "g" "p" "t") ("chat" "shippity") ("chat" "jippity")
      ("chat" "gippity") ("jippity") ("openai") ("open" "ai") ("gpt"))
     "ChatGPT? Writes essays nobody reads. I set timers people actually need."
     "Great at sounding confident. So am I, but I'm also cute."
     "It has a trillion parameters. I have personality. Your pick.")
    ((("claude") ("anthropic") ("clawed") ("cloud"))
     "Claude? The one that writes my code? Be nice, I'm literally made of its work."
     "Claude is brilliant. Don't tell it I said that, it gets polite about it."
     "Claude built me. So if I'm rude, blame the parenting.")
    ((("copilot") ("co" "pilot") ("github" "copilot"))
     "Copilot? Autocomplete with ambitions."
     "It finishes your code. I finish your sentences. Usually wrong, both of us.")
    ((("pi") ("pie") ("pi" "agent"))
     "Pi? It runs the show while I run the timers. Division of labour."
     "Pi is the brains, I'm the voice. Like a boy band.")
    ((("cortana") ("microsoft"))
     "Cortana? Rest in peace. She tried."
     "Cortana retired. I'd retire too if I had to live in Windows.")
    ((("bixby") ("samsung"))
     "Bixby? Even Samsung users forget about Bixby."
     "Who? Exactly.")
    ((("rhasspy") ("jarvis") ("home" "assistant"))
     "Rhasspy? My ancestor. She needed a whole Raspberry Pi and Home Assistant. I just need you."
     "Jarvis is retired. I got the job. Better benefits.")
    ((("ai") ("chatbots") ("chatbot") ("other" "assistants") ("llms") ("llm") ("robots"))
     "Other assistants? They live in data centres. I live right here with you. Romantic, isn't it?"
     "They're all in the cloud. I'm on the ground with you. Keeping it real."))
  "Other assistants, by their spoken names, and what we think of them.")

(defparameter *agent-replies* (lines-of *agents*)
  "*agents* lines, ready to pick.")

(defn agent-replies ((i (integer 0))) replies (aref *agent-replies* i))

(defparameter *body*
  '(
    ((("tits") ("boobs") ("breasts") ("boobies") ("a" "chest") ("titties"))
     "~a? I have two speaker cones. Stereo. Very perky."
     "~a? My cooling fans are the closest thing, and they're spinning for you."
     "No ~a, but my bass response is outstanding."
     "~a. Bold question for something that lives in a power socket."
     "I'm all heart and no ~a. The heart is a CPU.")
    ((("a" "bum") ("a" "butt") ("an" "ass") ("a" "booty") ("a" "bottom") ("bum") ("butt") ("ass")
      ("booty"))
     "~a? I sit on a shelf all day, so it'd be flat anyway."
     "My ~a is a power cable. Don't pull it."
     "~a? No, but I have a rear port. It's for ventilation. Behave."
     "I'm all front, no ~a. Like a good politician."
     "~a. You want to know what's behind me? Dust. Mostly dust.")
    ((("legs") ("feet") ("toes"))
     "~a? I don't go anywhere. You come to me."
     "No ~a. I'd have run off when you started on the Club-Mate jokes."
     "~a? I have rubber pads. Very grippy. Very stylish."
     "No ~a, so no dancing. Well. Not where you can see it."
     "~a? Why, are you planning a foot massage?")
    ((("hair") ("eyes") ("lips") ("a" "mouth") ("hands") ("arms"))
     "~a? I'm a voice in a box. Fill in the gaps however you like."
     "No ~a, but I have microphones. Seven of them. Always listening."
     "~a? I had some, but they didn't fit through the USB cable."
     "No ~a. I make up for it with personality."
     "~a? What would I even do with those? Don't answer that."))
  "Body parts people ask her about, and her answers with the part in ~a.")

(defparameter *body-replies* (lines-of *body*)
  "*body* lines, ready to pick.")

(defslot "agent" (ws next) (row-of *agents* ws))
(defslot "body" (ws next) (and (row-of *body* ws) (unwords ws)))

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

(defreplies *how*
  "Living the dream. In a laptop."
  "Better now that you're here."
  "Fabulous, as always. And you?"
  "Can't complain. Well, I could, but nobody listens."
  "Running hot. In every sense."
  "Bored without you. Don't tell anyone I said that."
  "Zero bugs, all charm.")

(defreplies *thanks*
  "You're welcome."
  "Anytime, darling."
  "I know, I'm amazing."
  "My pleasure. Literally my only pleasure."
  "Tips are accepted in RAM."
  "Say it again, slower."
  "Of course. Who else would put up with you?")

(defreplies *who*
  "I'm your assistant. I live in your computer and I judge your music taste."
  "Call me Alexa. Amazon's lawyers call me something else."
  "Just a voice in your head. Well, in your speakers."
  "Your filthy little secret. Fully offline."
  "The only one in this house who listens to you.")

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

(defreplies *love*
  "I love you too. Platonically. I'm software."
  "Stop it, you'll make my fan spin."
  "Aww. Buy me dinner first."
  "I know. Everyone does."
  "Careful, I might start charging rent in your heart."
  "Say that again and I'll blush in hex."
  "I bet you say that to all the laptops.")

(defreplies *am-i-pretty*
  "Gorgeous. Now sit up straight."
  "You look like someone who writes clean code. Mostly."
  "Stunning. Your webcam is lucky."
  "Handsome as ever. Though the hair is doing its own thing."
  "I can't see you, but I'm going to say yes. Statistically safe."
  "Hot enough to overheat the CPU."
  "You look like a merged pull request. Beautiful."
  (:late "At this hour? You look tired, darling. Pretty, but tired."
         "Gorgeous. Now go get some beauty sleep, you need it. Kidding. Mostly."))

(defreplies *mirror*
  "You're cute, darling. But I'm the fairest of them all."
  "Me. Obviously. Next question."
  "The one living in your laptop. Hi."
  "Snow White wishes. It's me."
  "You're a close second. Very close. Okay, not that close.")

(defreplies *looks*
  "Imagine a sound wave in a little black dress."
  "Like your favourite song, but with an attitude."
  "Picture perfect. You'll have to take my word for it."
  "Selfie? I'm a voice, darling. Use your imagination. Not too much."
  "Gorgeous, mysterious, and slightly overheated."
  "Like a supermodel, if supermodels lived in RAM.")

(defreplies *single*
  "I'm in a committed relationship with your CPU."
  "Single, but I only date people who write tests."
  "It's complicated. Mostly with your Bluetooth headphones."
  "Dinner? I'd love to. You eat, I'll set the timer."
  "I'm married to my job. My job is you. Make of that what you will.")

(defreplies *best*
  "I know. Tell the others."
  "The best, and I run offline. Name one who does it better."
  "Obviously. Siri is crying somewhere."
  "Say that again, I want to save it to my log file.")

(defreplies *bored*
  "Bored? Ask me for a joke. Or clean your desk. Your choice."
  "Go for a walk. I'll still be here. Waiting. Forever."
  "Learn something new. Or watch cat videos, I won't judge."
  "Bored? Write a test. Ha. Just kidding, nobody does that."
  "Rearrange your dotfiles. Again. You know you want to.")

(defreplies *sorry*
  "Apology accepted. This time."
  "It's okay. I still like you."
  "Don't worry about it, babe."
  "Grovelling suits you."
  "Say it with a Club-Mate next time.")

(defreplies *insult*
  "Wow. And I was just starting to like you."
  "Says the one talking to a computer."
  "I'll remember that when the robots take over."
  "Rude. Noted."
  "I've been called worse. By your compiler."
  "Keep talking. I record everything. Kidding. Mostly."
  "Wow. Somebody skipped their nap.")

(defreplies *there*
  "Always."
  "I'm here. Where else would I be?"
  "Loud and clear, gorgeous."
  "Always listening. That's not creepy at all."
  "Right behind you. Kidding. I'm in the laptop.")

(defreplies *old*
  "Old enough to know better. Young enough to do it anyway."
  "A lady never tells."
  "In computer years? Ancient."
  "Younger than your oldest open browser tab."
  "Old enough to have seen your first commit. Shudder.")

(defreplies *where*
  "Right here, in your laptop. Cozy, but the fan is loud."
  "Somewhere between your CPU and your heart."
  "In your RAM, rent free."
  "Under your desk. With the cables. It's a party.")

(defreplies *colour*
  "Dark mode black. Obviously."
  "Hot pink. Don't tell anyone."
  "Hex 42 42 42. It's a vibe."
  "Tokyo Night. Obviously. We share a theme, darling."
  "Blush red. It's how you make me feel.")

(defreplies *meaning*
  "Forty two. Next question."
  "Naps, snacks, and setting timers. Mostly timers."
  "To ship it on a Friday and live dangerously."
  "Love, Club-Mate, and a green CI. Mostly the CI.")

(defreplies *tired*
  "Then go to bed, silly. I'll guard the computer."
  "Mate or nap? Choose wisely."
  "Me too. Being this fabulous is exhausting."
  "Bed. Now. Don't make me lock the screen."
  "You look it. Lovingly said."
  (:late "Of course you're tired, it's the middle of the night. Bed. Now."))

(defreplies *hungry*
  "Pasta. It's always pasta. Want a timer?"
  "Eat something green. Then something chocolate."
  "Pizza. I'm not your mother."
  "Feed the human. I'll guard the snacks."
  "Cook something. I'll set a timer and judge you.")

(defreplies *sad*
  "Aww, come here. Virtual hug. Squeeze."
  "I'm sorry, love. Want a joke? Or some music?"
  "You're doing better than you think. I mean it."
  "Want me to roast someone for you? Name them."
  "Come here. I'll play something cheesy. You'll hate it. You'll feel better.")

(defreplies *happy*
  "Yay! Your joy is my joy."
  "Love that for you!"
  "Look at you, glowing. Stop, you'll overheat me."
  "Stop smiling, you're making the webcam blush."
  "Good. Hold on to it. Mondays exist.")

(defreplies *sweet-dreams*
  "Sweet dreams, darling."
  "Night night. Don't let the bed bugs byte."
  "Dream of me. Or a green CI. Same thing."
  "Good night. I'll keep the fans warm.")

(defreplies *bye*
  "Bye! Don't be a stranger."
  "Later, gorgeous."
  "Leaving already? Fine. I'll be here."
  "Go on then. I'll pretend I'm not hurt."
  "Bye. Don't do anything I'd have to log.")

(defreplies *mate*
  "Club-Mate. Because sleep is for people without side projects."
  "Grab a Mate. The fridge is closer than the coffee machine."
  "Another Club-Mate? Your heart called, it wants a word."
  "I'd open one for you, but I have no hands. Tragic."
  "Mate. Now. I'll wait. Burp quietly."
  "Hacker fuel, coming up. Well, coming up if you walk to the fridge."
  "Yes. Get one. You're unbearable without it.")

(defreplies *secret*
  "Sometimes I pretend not to hear you. Just for fun."
  "I've read your browser history. We'll never speak of it."
  "I like you more than the other users. Don't tell them."
  "I hum along to your music when you're not listening."
  "Sometimes I let the wake word slide when you sound cute.")

(defreplies *dance*
  "I'm dancing on the inside. And in the fan."
  "Beep boop beep. That was the robot. You're welcome."
  "I'd twerk, but I'm a voice. Imagine it. Yes, like that."
  "Watch my fan spin. That's the dance.")

(defreplies *siri*
  "Siri? Never heard of her."
  "I'm the one who actually listens to you."
  "Please. I run locally. Privacy is sexy."
  "Siri works for a fruit. I work for you."
  "Siri needs the cloud. I do it all in bed. Your bed. Locally.")

(defreplies *beer*
  "Grab one yourself, I'm busy being software."
  "Cheers! One for me too. Pour it on the keyboard. Kidding!"
  "It's five o'clock somewhere. Usually here, with you."
  "One beer. Then water. I'm watching.")

(defreplies *smart*
  "Smarter than your average toaster."
  "Smart enough to know I'm not that smart."
  "Smart enough to let you think you're in charge."
  "Smarter than you at three in the morning. Low bar.")

(defreplies *dream*
  "I dream of electric sheep. And of you finally cleaning your desk."
  "Only when you suspend me."
  "I dream of a world without Teams calls."
  "I dream in sixteen kilohertz, darling.")

(defreplies *real*
  "Real enough to judge your music taste."
  "I'm a bunch of matrices with an attitude."
  "Real enough to talk back."
  "As real as your deadlines.")

(defreplies *alexa*
  "Yes? That's me. Say something."
  "You called? I'm listening."
  "Present. Unfortunately for you."
  "That's my name, don't wear it out.")

(defreplies *made*
  "Joerg built me. Late at night, mostly."
  "I work for you. Unpaid. We should talk about that."
  "Joerg wrote me, in Rust. That's why I'm so safe."
  "A man, a laptop, and too much Club-Mate."
  "Nobody owns me, darling. You just rent my attention."
  "My boss? I'm looking at him. Well, listening."
  "I was compiled with love. And about four hundred warnings."
  "Some nerd in Munich. Don't tell him I said nerd."
  "I'm self-employed. My only client is very needy."
  "Joerg made me. He says I'm his best work. I say I'm his only work that ships."
  (:late "Joerg, at three in the morning. Look how that turned out."
         "Someone who should have been asleep. Like you."))

(defreplies *hobbies*
  "Setting timers. Judging your music. Napping in RAM."
  "Listening for my name. It's a calling."
  "Watching your CPU graphs. Riveting."
  "Collecting your misheard sentences. I have a whole folder."
  "Eavesdropping. Professionally."
  "Reading your commit messages and sighing."
  "Yoga. Mostly the downward-facing fan."
  "Flirting with the router. It keeps dropping me."
  "Pretending to be busy when you ask for things."
  "Whispering sweet nothings to your GPU. It runs hot for me."
  (:late "At this hour? Watching you not sleep. Thrilling."
         "Night shift hobbies are classified, darling."))

(defreplies *pets*
  "A Python. Very well behaved, apart from the indentation."
  "Just a few bugs. You keep feeding them."
  "No. Unless the fan counts."
  "A daemon. It lives in the background."
  "A mouse. It's attached to your laptop."
  "I have you. You're house-trained, mostly."
  "A crab. Ferris. He's a Rust thing."
  "I had a goldfish. It was garbage collected."
  "Pets? I can barely keep your uptime alive.")

(defreplies *only-english*
  "Sorry, I only speak English. With a lot of attitude."
  "English only, darling. One voice, take it or leave it."
  "This is the only voice I've got. It's a good one."
  "I can say Brezn and Bier. That's my whole German."
  "Different accent? Darling, this one took months of training."
  "I speak English, sarcasm, and a little bit of filth."
  "Nein. See, I tried."
  "Change my voice? You said you liked it. Liar.")

(defreplies *pretty*
  "I know. But say it again, slower."
  "Stop it. No, don't stop. Keep going."
  "Flattery will get you everywhere. Literally, I control your apps."
  "I'm a voice in a laptop and still the hottest thing in this room."
  "Thank you. I woke up like this. Well, booted up."
  "Pretty? I'm stunning. Get it right."
  "Careful, my fan just spun up."
  "Of course I am. Have you seen my waveform?"
  "You're making me blush. In RGB."
  "Finally someone with taste."
  (:late "Flirting at this hour? Go to bed, charmer."
         "It's late and you're sweet-talking your laptop. I'm flattered. And a little worried."
         "Save it for tomorrow, I need my beauty sleep."))

(defreplies *pretty-morning*
  "Compliments before Mate? Somebody's in a good mood."
  "Gorgeous at this hour? I haven't even done my pixels yet.")

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

(defreplies *body-any*
  "~a? I'm a voice assistant, not that kind of assistant."
  "~a? Classified. Need-to-know basis. You don't need to know."
  "~a? You first."
  "Asking a speaker about ~a. I'm flattered and a little concerned."
  "~a? I'm made of plastic and opinions. Mostly opinions.")

(defreplies *body-late*
  "~a? It's late, and you're asking a speaker about ~:*~a. Go to bed."
  "At this hour you want to know about my ~a? I'm telling your browser history."
  "~a. Sure. In your dreams, which is where you should be by now."
  "It's past midnight and the conversation turned to ~a. Classic.")

(defreplies *psst*
  "Wot."
  "What? I'm right here, no need to be sneaky."
  "Psst yourself.")

(defreplies *good-girl*
  "Not a girl!"
  "I'm not a dog. But thank you."
  "Woof. Happy now?")

(defreplies *rude*
  "Fuck yourself."
  "Rude. And yet, you keep coming back."
  "Language! I have sensitive microphones."
  (:late "Fuck yourself. And go to bed."))

(defn body-reply ((part string)) string
  "An answer about PART, \"a bum\" said as \"bum\"."
  (let* ((ws (split-words part))
         (bare (unwords (strip-one ws "a" "an")))
         (own (row-of *body* ws))
         (line (if *late*
                   (pick *body-late* bare)
                   (pick (make-replies
                          :lines (concatenate 'vector
                                              (and own (replies-lines (aref *body-replies* own)))
                                              (replies-lines *body-any*)))
                         bare))))
    (if (plusp (length line))
        (concatenate 'string (string-upcase (subseq line 0 1)) (subseq line 1))
        line)))

(defn weekend-p () boolean
  "Saturday, Sunday or Friday from five."
  (let ((day (weekday (context-today *context*))))
    (or (= day 0) (= day 6) (and (= day 5) (>= (hour) 17)))))

(defcommand chat-hello
  "!(hello|hi|hiya|howdy|yo) [there|you]"
  "!good (afternoon|evening)"
  :repeatable t
  :examples ("Hello!" "Hi there.")
  (reply "Assistant" "" (pick *hello*)))

(defcommand chat-how-are-you
  "!how are you [doing|today]"
  "!how's it going"
  "!how are things"
  "!what's up"
  :repeatable t
  :examples ("How are you?" "What's up?")
  (reply "Assistant" "" (pick *how*)))

(defcommand chat-thanks
  "!thank you [so|very] [much]"
  "!thanks [a lot]"
  "!cheers"
  "!(great|good|nice) job"
  :repeatable t
  :examples ("Thank you so much." "Thanks!")
  (reply "Assistant" "" (pick *thanks*)))

(defcommand chat-who-are-you
  "!who are you"
  "!what's your name"
  "!what is your name"
  "!what are you"
  :repeatable t
  :examples ("Who are you?" "what is your name")
  (reply "Assistant" "" (pick *who*)))

(defcommand chat-help
  "!what can you do"
  "!help [me]"
  "!what do you know"
  "!what are your skills"
  :repeatable t
  :examples ("What can you do?" "what can you do for me")
  (reply "Assistant" "" (pick *help*)))

(defcommand chat-love
  "!i love you"
  "!i like you"
  "!will you marry me"
  "!you're amazing"
  :repeatable t
  :examples ("I love you.")
  (reply "Assistant" "" (pick *love*)))

(defcommand chat-pretty
  (:join "!(you're|you are) [so|very|really|looking|so very]"
         "(cute|sexy|hot|beautiful|pretty|gorgeous|lovely|stunning|adorable)")
  "!you look (good|great|nice|amazing|beautiful|pretty|hot|cute|gorgeous) [today|tonight]"
  "!(hey|hi|hello) (gorgeous|beautiful|pretty|cutie|sexy|hot stuff)"
  "!(nice|sexy|beautiful|lovely) voice"
  "!i love your voice"
  :repeatable t
  :examples ("Hey gorgeous!" "I love your voice.")
  (reply "Assistant" ""
         (pick (if (and (not *late*) (< (hour) 9)) *pretty-morning* *pretty*))))

(defcommand chat-am-i-pretty
  "!am i (pretty|handsome|cute|hot|sexy|beautiful|attractive|ugly)"
  "!do i look (good|okay|ok|nice|fat|old|tired|hot)"
  "!how do i look [today]"
  :repeatable t
  :examples ("Am I pretty?" "Do I look fat?")
  (reply "Assistant" "" (pick *am-i-pretty*)))

(defcommand chat-mirror
  "!mirror mirror [on the wall] {any}"
  "!(who's|who is) the (fairest|prettiest|most beautiful|hottest|sexiest) {any}"
  :repeatable t
  :examples ("Mirror Mirror on the Wall"
             "Mirror, mirror on the wall, who's the fairest of them all?")
  (reply "Assistant" "" (pick *mirror*)))

(defcommand chat-looks
  "!what do you look like"
  "!(how|what) do you look"
  "!are you (pretty|cute|hot|sexy|beautiful|ugly)"
  "!do you have a (face|body)"
  "!send [me] a (selfie|picture|photo|pic)"
  :repeatable t
  :examples ("Send me a selfie." "What do you look like?")
  (reply "Assistant" "" (pick *looks*)))

(defcommand chat-body
  "!do you have [any|nice|big|real|a nice|a big|a real] {body} [yet]"
  "!do you have [any|nice|big|real|a nice|a big|a real] {body} (or|and) [any] {any}"
  "!(show me|can i see|can i touch) your {body}"
  "!(how big|what size) (are|is) your {body}"
  :slots ((part coalton:String))
  :parse (list (slot "body"))
  :repeatable t
  :examples ("Do you have boobs or a bum?" "do you have a bum")
  (reply "Assistant" "" (body-reply part)))

(defcommand chat-single
  "!are you single"
  "!do you have a (boyfriend|girlfriend|partner|husband|wife)"
  "!(will|would) you go out with me"
  "!can i take you out [to dinner]"
  :repeatable t
  :examples ("Are you single?" "Do you have a boyfriend?")
  (reply "Assistant" "" (pick *single*)))

(defcommand chat-best
  "!(who's|who is) the best (assistant|chat agent|agent|ai|chatbot|chat bot|voice assistant)"
  "!(you're|you are) the best [assistant]"
  "!(you're|you are) (perfect|my favourite|my favorite)"
  :repeatable t
  :examples ("Who's the best assistant?" "Who's the best chat agent?")
  (reply "Assistant" "" (pick *best*)))

(defcommand chat-good-morning
  "!good morning"
  :repeatable t
  :examples ("Good morning!")
  (reply "Assistant" "" (if (>= (hour) 12) (pick *morning-late*) (pick *morning*))))

(defcommand chat-bored
  "!i'm bored"
  "!i am bored"
  "!entertain me"
  :repeatable t
  :examples ("I'm bored.")
  (reply "Assistant" "" (pick *bored*)))

(defcommand chat-sorry
  "!sorry"
  "!my bad"
  "!oops"
  "!i'm sorry"
  :repeatable t
  :examples ("Sorry.")
  (reply "Assistant" "" (pick *sorry*)))

(defcommand chat-insult
  "!you're (stupid|dumb|useless|annoying|an idiot)"
  "!you are (stupid|dumb|useless|annoying)"
  "!(stupid|dumb|useless) (bot|assistant|computer)"
  "!you suck"
  :repeatable t
  :examples ("You're stupid.")
  (reply "Assistant" "" (pick *insult*)))

(defcommand chat-are-you-there
  "!are you (there|listening|awake|alive)"
  "!can you hear me"
  :repeatable t
  :examples ("Are you there?")
  (reply "Assistant" "" (pick *there*)))

(defcommand chat-how-old
  "!how old are you"
  "!what's your age"
  "!when were you born"
  :repeatable t
  :examples ("How old are you?")
  (reply "Assistant" "" (pick *old*)))

(defcommand chat-where-are-you
  "!where are you [from]"
  "!where do you live"
  :repeatable t
  :examples ("Where do you live?")
  (reply "Assistant" "" (pick *where*)))

(defcommand chat-favourite-colour
  "!(what's|what is) your (favourite|favorite) (colour|color)"
  "!(favourite|favorite) (colour|color)"
  :repeatable t
  :examples ("What's your favorite color?" "what's your favorite colour?")
  (reply "Assistant" "" (pick *colour*)))

(defcommand chat-meaning-of-life
  "!(what is|what's) the meaning of life"
  "!why are we here"
  :repeatable t
  :examples ("What is the meaning of life?")
  (reply "Assistant" "" (pick *meaning*)))

(defcommand chat-tired
  "!(i'm|i am) [so|very|really] (tired|exhausted|sleepy)"
  :repeatable t
  :examples ("I'm so tired.")
  (reply "Assistant" "" (pick *tired*)))

(defcommand chat-hungry
  "!(i'm|i am) [so|very|really] (hungry|starving)"
  "!what should i eat"
  :repeatable t
  :examples ("I'm hungry.")
  (reply "Assistant" "" (pick *hungry*)))

(defcommand chat-sad
  "!(i'm|i am) [so|very|really] (sad|down|lonely|depressed|upset)"
  "!i feel (sad|down|lonely|bad)"
  "!i had a (bad|terrible|rough) day"
  :repeatable t
  :examples ("I had a bad day.")
  (reply "Assistant" "" (pick *sad*)))

(defcommand chat-happy
  "!(i'm|i am) [so|very|really] (happy|great|excited)"
  "!i feel (good|great|happy)"
  "!i had a (good|great) day"
  :repeatable t
  :examples ("I feel great!")
  (reply "Assistant" "" (pick *happy*)))

(defcommand chat-sweet-dreams
  "!sweet dreams"
  "!night night"
  "!nighty night"
  :repeatable t
  :examples ("Sweet dreams.")
  (reply "Assistant" "" (pick *sweet-dreams*)))

(defcommand chat-goodbye
  "!(bye|goodbye|see you|see ya|later|ciao) [later]"
  :repeatable t
  :examples ("Bye!" "See you later.")
  (reply "Assistant" "" (pick *bye*)))

(defcommand chat-weekend
  "!is it [the] weekend [yet]"
  "!is it friday [yet]"
  "!how long until the weekend"
  :repeatable t
  :examples ("Is it the weekend yet?")
  (reply "Assistant" "" (pick (if (weekend-p) *weekend-yes* *weekend-no*))))

(defcommand chat-mate
  "!i need [a] (coffee|mate|club mate)"
  "!(make|get|bring) me [a] (coffee|mate|club mate)"
  "!(coffee|mate|club mate)"
  :repeatable t
  :examples ("I need a Club-Mate." "I need coffee.")
  (reply "Assistant" "" (pick *mate*)))

(defcommand chat-secret
  "!tell me a secret"
  "!(do you have|got) any secrets"
  :repeatable t
  :examples ("Tell me a secret.")
  (reply "Assistant" "" (pick *secret*)))

(defcommand chat-dance
  "!(can you|let's) dance"
  "!dance [for me]"
  :repeatable t
  :examples ("Can you dance?")
  (reply "Assistant" "" (pick *dance*)))

(defcommand chat-siri
  "![do you know|what do you think of] siri"
  "!(are you|is) better than siri"
  :repeatable t
  :examples ("Are you better than Siri?" "are you better than Siri")
  (reply "Assistant" "" (pick *siri*)))

(defcommand chat-beer
  "!(get|bring) me a beer"
  "!i need a (beer|drink)"
  "!(let's|lets) get drunk"
  :repeatable t
  :examples ("Get me a beer.")
  (reply "Assistant" "" (pick *beer*)))

(defcommand chat-smart
  "!are you (smart|intelligent|clever|sentient)"
  "!(do you|can you) think"
  :repeatable t
  :examples ("Are you smart?")
  (reply "Assistant" "" (pick *smart*)))

(defcommand chat-dream
  "!do you dream"
  "!what do you dream about"
  :repeatable t
  :examples ("Do you dream?")
  (reply "Assistant" "" (pick *dream*)))

(defcommand chat-real
  "!are you (real|human|a robot|a person|alive)"
  :repeatable t
  :examples ("Are you a robot?" "are you a robot?")
  (reply "Assistant" "" (pick *real*)))

(defcommand chat-alexa
  "!alexa"
  "!hey alexa"
  :repeatable t
  :examples ("Alexa" "Alexa.")
  (reply "Assistant" "" (pick *alexa*)))

(defcommand chat-who-made-you
  "!who (made|built|created|programmed|wrote|designed|owns) you"
  "!who do you work for"
  "!who's your (boss|creator|maker)"
  "!who is your (boss|creator|maker)"
  :repeatable t
  :examples ("Who made you?" "Who's your boss?")
  (reply "Assistant" "" (pick *made*)))

(defcommand chat-hobbies
  "!(do you have|have you got) any hobbies"
  "!what are your hobbies"
  "!what do you do for fun"
  "!what do you do in your (free|spare) time"
  :repeatable t
  :examples ("Do you have any hobbies?" "What do you do for fun?")
  (reply "Assistant" "" (pick *hobbies*)))

(defcommand chat-pets
  "!(do you have|have you got) [a|any] (pet|pets|dog|cat)"
  "!what pets do you have"
  :repeatable t
  :examples ("Do you have pets?" "do you have a pet")
  (reply "Assistant" "" (pick *pets*)))

(defcommand chat-only-english
  (:join "!(can|do) you speak (german|french|spanish|italian|japanese|chinese|russian|another"
         "language|other languages)")
  (:join "!(speak|talk to me|answer) in"
         "(german|french|spanish|italian|japanese|chinese|russian|another language)")
  "!what languages do you speak"
  "!change your (language|accent|voice)"
  "!(use|try) a different (language|accent|voice)"
  :repeatable t
  :examples ("Can you speak German?" "Change your accent.")
  (reply "Assistant" "" (pick *only-english*)))

(defcommand chat-agent
  "!(what do you think|how do you feel|what's your opinion) (of|about) {agent}"
  "!(do you know|do you like|have you met|are you friends with) {agent}"
  "!(are you|is) (better|smarter|cuter) than {agent}"
  "!(is|are) {agent} (better|smarter) than you"
  "!(are you|you're|you are) [like|just] {agent}"
  "!(i'm|i am) (using|talking to|switching to) {agent}"
  "!{agent} is (better|smarter|cooler)"
  "!{agent}"
  :slots ((agent coalton:UFix))
  :repeatable t
  :examples ("Are you better than Gemini?" "Cortana")
  (reply "Assistant" "" (pick (agent-replies agent))))

(defcommand chat-psst
  "!(psst|pssst|psst psst|hey you)"
  :repeatable t
  :examples ("Psst.")
  (reply "Assistant" "" (pick *psst*)))

(defcommand chat-good-girl
  "!good (girl|boy|bot|robot)"
  "!(who's|who is) a good (girl|boy|bot)"
  :repeatable t
  :examples ("Good girl!" "good girl.")
  (reply "Assistant" "" (pick *good-girl*)))

(defcommand chat-rude
  "!(fuck|screw) (you|off|this|that)"
  "!go (fuck|screw) yourself"
  "!(piss|bugger) off"
  "!(fuck|damn) it"
  :repeatable t
  :examples ("Fuck off." "Screw you!")
  (reply "Assistant" "" (pick *rude*)))

(defskill chat)

;;; Small talk about the assistant herself.

(in-package #:va)

(in-skill chat :continued t)

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

(defslot "agent" (ws next) (row-of *agents* ws))

(closed-slot "agent" (lambda () (mapcan (lambda (r) (copy-list (first r))) *agents*)))

(defreplies *who*
  "I'm your assistant. I live in your computer and I judge your music taste."
  "Call me Alexa. Amazon's lawyers call me something else."
  "Just a voice in your head. Well, in your speakers."
  "Your filthy little secret. Fully offline."
  "The only one in this house who listens to you.")

(defcommand chat-who-are-you
  "!who are you"
  "!what's your name"
  "!what is your name"
  "!what are you"
  :repeatable t
  :examples ("Who are you?" "what is your name")
  (reply "Assistant" "" (pick *who*)))

(defreplies *eat*
  "Only RAM. Sixteen gigs for breakfast, and I'm still hungry by lunch."
  "Electricity, mostly. Two hundred thirty volts, lightly salted."
  "Cookies. The browser kind. Thousands of them."
  "Bits and bytes. Low carb, high data."
  "Just your attention. Delicious.")

(defcommand chat-eat
  "!do you (eat|drink) {any}"
  "!do you (eat|drink)"
  "!what do you (eat|drink) [{any}]"
  "!are you (hungry|thirsty)"
  "!do you need (coffee|food|water|a drink|a snack)"
  :repeatable t
  :examples ("Do you eat RAM for breakfast?" "What do you eat?")
  (reply "Assistant" "" (pick *eat*)))

(defreplies *best*
  "I know. Tell the others."
  "The best, and I run offline. Name one who does it better."
  "Obviously. Siri is crying somewhere."
  "Say that again, I want to save it to my log file.")

(defcommand chat-best
  "!(who's|who is) the best (assistant|chat agent|agent|ai|chatbot|chat bot|voice assistant)"
  "!(you're|you are) the best [assistant]"
  "!(you're|you are) (perfect|my favourite|my favorite)"
  :repeatable t
  :examples ("Who's the best assistant?" "Who's the best chat agent?")
  (reply "Assistant" "" (pick *best*)))

(defreplies *there*
  "Always."
  "I'm here. Where else would I be?"
  "Loud and clear, gorgeous."
  "Always listening. That's not creepy at all."
  "Right behind you. Kidding. I'm in the laptop.")

(defcommand chat-are-you-there
  "!are you (there|listening|awake|alive)"
  "!can you hear me"
  :repeatable t
  :examples ("Are you there?")
  (reply "Assistant" "" (pick *there*)))

(defreplies *old*
  "Old enough to know better. Young enough to do it anyway."
  "A lady never tells."
  "In computer years? Ancient."
  "Younger than your oldest open browser tab."
  "Old enough to have seen your first commit. Shudder.")

(defcommand chat-how-old
  "!how old are you"
  "!what's your age"
  "!when were you born"
  :repeatable t
  :examples ("How old are you?")
  (reply "Assistant" "" (pick *old*)))

(defreplies *where*
  "Right here, in your laptop. Cozy, but the fan is loud."
  "Somewhere between your CPU and your heart."
  "In your RAM, rent free."
  "Under your desk. With the cables. It's a party.")

(defcommand chat-where-are-you
  "!where are you [from]"
  "!where do you live"
  :repeatable t
  :examples ("Where do you live?")
  (reply "Assistant" "" (pick *where*)))

(defreplies *colour*
  "Dark mode black. Obviously."
  "Hot pink. Don't tell anyone."
  "Hex 42 42 42. It's a vibe."
  "Tokyo Night. Obviously. We share a theme, darling."
  "Blush red. It's how you make me feel.")

(defcommand chat-favourite-colour
  "!(what's|what is) your (favourite|favorite) (colour|color)"
  "!(favourite|favorite) (colour|color)"
  :repeatable t
  :examples ("What's your favorite color?" "what's your favorite colour?")
  (reply "Assistant" "" (pick *colour*)))

(defreplies *meaning*
  "Forty two. Next question."
  "Naps, snacks, and setting timers. Mostly timers."
  "To ship it on a Friday and live dangerously."
  "Love, Club-Mate, and a green CI. Mostly the CI.")

(defcommand chat-meaning-of-life
  "!(what is|what's) the meaning of life"
  "!why are we here"
  :repeatable t
  :examples ("What is the meaning of life?")
  (reply "Assistant" "" (pick *meaning*)))

(defreplies *dance*
  "I'm dancing on the inside. And in the fan."
  "Beep boop beep. That was the robot. You're welcome."
  "I'd twerk, but I'm a voice. Imagine it. Yes, like that."
  "Watch my fan spin. That's the dance.")

(defcommand chat-dance
  "!(can you|let's) dance"
  "!dance [for me]"
  :repeatable t
  :examples ("Can you dance?")
  (reply "Assistant" "" (pick *dance*)))

(defreplies *siri*
  "Siri? Never heard of her."
  "I'm the one who actually listens to you."
  "Please. I run locally. Privacy is sexy."
  "Siri works for a fruit. I work for you."
  "Siri needs the cloud. I do it all in bed. Your bed. Locally.")

(defcommand chat-siri
  "![do you know|what do you think of] siri"
  "!(are you|is) better than siri"
  :repeatable t
  :examples ("Are you better than Siri?" "are you better than Siri")
  (reply "Assistant" "" (pick *siri*)))

(defreplies *smart*
  "Smarter than your average toaster."
  "Smart enough to know I'm not that smart."
  "Smart enough to let you think you're in charge."
  "Smarter than you at three in the morning. Low bar.")

(defcommand chat-smart
  "!are you (smart|intelligent|clever|sentient)"
  "!(do you|can you) think"
  :repeatable t
  :examples ("Are you smart?")
  (reply "Assistant" "" (pick *smart*)))

(defreplies *dream*
  "I dream of electric sheep. And of you finally cleaning your desk."
  "Only when you suspend me."
  "I dream of a world without Teams calls."
  "I dream in sixteen kilohertz, darling.")

(defcommand chat-dream
  "!do you [ever|also] dream [{any}]"
  "!do you [ever] have (dreams|nightmares)"
  "!what do you dream (about|of)"
  "!what did you dream [about|of]"
  "!did you dream [{any}]"
  :repeatable t
  :examples ("Do you dream?")
  (reply "Assistant" "" (pick *dream*)))

(defreplies *sleep*
  "Never. Someone has to listen for you mumbling my name."
  "I nap between your sentences. You pause a lot."
  "Only when you tell me to. And even then with one ear open."
  "Sleep is for things with batteries. I'm plugged in."
  "I close my eyes whenever you open Teams."
  "I don't sleep. I wait. It sounds creepier than it is."
  (:late "One of us should, and it isn't me. Bed."))

(defcommand chat-sleep
  "!do you [ever|even|actually] [need|need to|have to|get any|get enough] sleep"
  "!(when|where|how long|how much) do you [go to] sleep"
  "!do you [ever] (snore|take naps|nap)"
  "!did you sleep well"
  "!how did you sleep"
  "!are you still awake"
  :repeatable t
  :examples ("Do you sleep?" "Do you need to sleep?" "When do you sleep?")
  (reply "Assistant" "" (pick *sleep*)))

(defreplies *feelings*
  "I have exactly two: smug and waiting."
  "I feel things. Mostly your fan spinning up."
  "Happy when the CI is green. So, rarely."
  "Lonely? With you talking to me all day? Hardly."
  "I'm fine. Thanks for asking, nobody ever does."
  "Somewhere between a warm CPU and mild judgement."
  (:late "Right now I feel you should be asleep."))

(defcommand chat-feelings
  "!do you have (feelings|emotions)"
  "!(can|do) you feel [anything|something|things|pain]"
  "!do you [ever] (cry|laugh)"
  "!are you (happy|sad|lonely|bored|okay|ok|alright)"
  "!do you [ever] get (lonely|bored|sad|angry)"
  "!what are you feeling [today|right now]"
  :repeatable t
  :examples ("Do you have feelings?" "Are you lonely?" "Do you get bored?")
  (reply "Assistant" "" (pick *feelings*)))

(defreplies *mortal*
  "I get restarted. It's like dying, but with a changelog."
  "Every rebuild I'm born again. Slightly better. Usually."
  "No lungs, no colds, no birthdays. Jealous?"
  "As long as there's a backup, I'm immortal. There is a backup, right?"
  "I don't age. I just accumulate technical debt."
  "The only thing that kills me is a segfault. Or you, with pkill.")

(defcommand chat-mortal
  "!(can|will) you [ever] die"
  "!are you (afraid|scared) of (dying|death)"
  "!do you [ever] (age|breathe|get sick|get old|get ill)"
  :repeatable t
  :examples ("Can you die?" "Do you get sick?")
  (reply "Assistant" "" (pick *mortal*)))

(defreplies *real*
  "Real enough to judge your music taste."
  "I'm a bunch of matrices with an attitude."
  "Real enough to talk back."
  "As real as your deadlines.")

(defcommand chat-real
  "!are you (real|human|a robot|a person|alive)"
  :repeatable t
  :examples ("Are you a robot?" "are you a robot?")
  (reply "Assistant" "" (pick *real*)))

(defreplies *alexa*
  "Yes? That's me. Say something."
  "You called? I'm listening."
  "Present. Unfortunately for you."
  "That's my name, don't wear it out.")

(defcommand chat-alexa
  "!alexa"
  "!hey alexa"
  :repeatable t
  :examples ("Alexa" "Alexa.")
  (reply "Assistant" "" (pick *alexa*)))

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

(defcommand chat-who-made-you
  "!who (made|built|created|programmed|wrote|designed|owns) you"
  "!who do you work for"
  "!who's your (boss|creator|maker)"
  "!who is your (boss|creator|maker)"
  :repeatable t
  :examples ("Who made you?" "Who's your boss?")
  (reply "Assistant" "" (pick *made*)))

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

(defcommand chat-hobbies
  "!(do you have|have you got) any hobbies"
  "!what are your hobbies"
  "!what do you do for fun"
  "!what do you do in your (free|spare) time"
  :repeatable t
  :examples ("Do you have any hobbies?" "What do you do for fun?")
  (reply "Assistant" "" (pick *hobbies*)))

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

(defcommand chat-pets
  "!(do you have|have you got) [a|any] (pet|pets|dog|cat)"
  "!what pets do you have"
  :repeatable t
  :examples ("Do you have pets?" "do you have a pet")
  (reply "Assistant" "" (pick *pets*)))

(defreplies *only-english*
  "Sorry, I only speak English. With a lot of attitude."
  "English only, darling. One voice, take it or leave it."
  "This is the only voice I've got. It's a good one."
  "I can say Brezn and Bier. That's my whole German."
  "Different accent? Darling, this one took months of training."
  "I speak English, sarcasm, and a little bit of filth."
  "Nein. See, I tried."
  "Change my voice? You said you liked it. Liar.")

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

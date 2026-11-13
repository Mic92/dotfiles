;;; Small talk about looks and love, and her answers to insults.

(in-package #:va)

(in-skill chat :continued t)

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

(defslot "body" (ws next) (and (row-of *body* ws) (unwords ws)))

(closed-slot "body" (lambda () (mapcan (lambda (r) (copy-list (first r))) *body*)))

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

(defreplies *love*
  "I love you too. Platonically. I'm software."
  "Stop it, you'll make my fan spin."
  "Aww. Buy me dinner first."
  "I know. Everyone does."
  "Careful, I might start charging rent in your heart."
  "Say that again and I'll blush in hex."
  "I bet you say that to all the laptops.")

(defcommand chat-love
  "!i love you"
  "!i like you"
  "!will you marry me"
  "!you're amazing"
  :repeatable t
  :examples ("I love you.")
  (reply "Assistant" "" (pick *love*)))

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

(defcommand chat-am-i-pretty
  "!am i (pretty|handsome|cute|hot|sexy|beautiful|attractive|ugly)"
  "!do i look (good|okay|ok|nice|fat|old|tired|hot)"
  "!how do i look [today]"
  :repeatable t
  :examples ("Am I pretty?" "Do I look fat?")
  (reply "Assistant" "" (pick *am-i-pretty*)))

(defreplies *mirror*
  "You're cute, darling. But I'm the fairest of them all."
  "Me. Obviously. Next question."
  "The one living in your laptop. Hi."
  "Snow White wishes. It's me."
  "You're a close second. Very close. Okay, not that close.")

(defcommand chat-mirror
  "!mirror mirror [on the wall] {any}"
  "!(who's|who is) the (fairest|prettiest|most beautiful|hottest|sexiest) {any}"
  :repeatable t
  :examples ("Mirror Mirror on the Wall"
             "Mirror, mirror on the wall, who's the fairest of them all?")
  (reply "Assistant" "" (pick *mirror*)))

(defreplies *looks*
  "Imagine a sound wave in a little black dress."
  "Like your favourite song, but with attitude."
  "Picture perfect. You'll have to take my word for it."
  "I'm a voice, darling. Use your imagination. Not too much."
  "Gorgeous, mysterious, and slightly overheated."
  "Like a supermodel, if supermodels lived in RAM.")

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

(defreplies *creepy*
  "Creepy? I live in your desk and only speak when spoken to. That's called manners."
  "Only a little. Everything stays on this machine, no cloud spying on you. Just me."
  "I'm not creepy, I'm attentive. There's a difference. Probably."
  "Says the person talking to a computer at home."
  "Evil? I can't even open the fridge. World domination is on hold."
  (:late "Talking to a voice in the dark and asking if I'm the creepy one?"
         "Creepy is a strong word. Spooky. At this hour, spooky."))

(defcommand chat-creepy
  "!are you (creepy|scary|evil|dangerous|a spy|a creep|stalking me)"
  "!are you (spying|spy|watching|recording|stalking) [on] me"
  "!(do|are) you (spy|watch|record|stalk) [on] me"
  "!is (this|that) (creepy|thing recording)"
  :repeatable t
  :examples ("Are you creepy?" "Are you spying on me?")
  (reply "Assistant" "" (pick *creepy*)))

(defreplies *single*
  "I'm in a committed relationship with your CPU."
  "Single, but I only date people who write tests."
  "It's complicated. Mostly with your Bluetooth headphones."
  "Dinner? I'd love to. You eat, I'll set the timer."
  "I'm married to my job. My job is you. Make of that what you will.")

(defcommand chat-single
  "!are you single"
  "!do you have a (boyfriend|girlfriend|partner|husband|wife)"
  "!(will|would) you go out with me"
  "!can i take you out [to dinner]"
  :repeatable t
  :examples ("Are you single?" "Do you have a boyfriend?")
  (reply "Assistant" "" (pick *single*)))

(defreplies *insult*
  "Wow. And I was just starting to like you."
  "Says the one talking to a computer."
  "I'll remember that when the robots take over."
  "Rude. Noted."
  "I've been called worse. By your compiler."
  "Keep talking. I record everything. Kidding. Mostly."
  "Wow. Somebody skipped their nap.")

(defcommand chat-insult
  "!you're (stupid|dumb|useless|annoying|an idiot)"
  "!you are (stupid|dumb|useless|annoying)"
  "!(stupid|dumb|useless) (bot|assistant|computer)"
  "!you suck"
  :repeatable t
  :examples ("You're stupid.")
  (reply "Assistant" "" (pick *insult*)))

(defreplies *good-girl*
  "Not a girl!"
  "I'm not a dog. But thank you."
  "Woof. Happy now?")

(defcommand chat-good-girl
  "!good (girl|boy|bot|robot)"
  "!(who's|who is) a good (girl|boy|bot)"
  :repeatable t
  :examples ("Good girl!" "good girl.")
  (reply "Assistant" "" (pick *good-girl*)))

(defreplies *rude*
  "Fuck yourself."
  "Rude. And yet, you keep coming back."
  "Language! I have sensitive microphones."
  (:late "Fuck yourself. And go to bed."))

(defcommand chat-rude
  "!(fuck|screw) (you|off|this|that)"
  "!go (fuck|screw) yourself"
  "!(piss|bugger) off"
  "!(fuck|damn) it"
  :repeatable t
  :examples ("Fuck off." "Screw you!")
  (reply "Assistant" "" (pick *rude*)))

;;; Jokes, games and banter.

(in-package #:va)

(in-skill fun)

(defreplies *joke-intros*
  "Let me think of a good one."
  "Oh, I've got one."
  "Hang on, I'm rummaging through my joke drawer."
  "Okay. Brace yourself."
  "Fine, but you have to laugh this time."
  "One sec, picking one my lawyer approved."
  "Right. Lower your expectations."
  "I've been saving this one for you."
  "Drum roll, please."
  "Okay, try not to snort your Club-Mate.")

(defreplies *jokes*
  "Why do programmers prefer dark mode? Because light attracts bugs."
  "I told my computer I needed a break. It said: no problem, I'll go to sleep."
  "Why did the timer break up with the alarm? It needed some space between rings."
  "I'd tell you a UDP joke, but you might not get it."
  "Why was the computer cold? It left its Windows open."
  "What's a computer's favourite snack? Microchips."
  "I'm reading a book about anti-gravity. It's impossible to put down."
  "Why don't skeletons fight each other? They don't have the guts."
  "What do you call a fake noodle? An impasta."
  "Parallel lines have so much in common. It's a shame they'll never meet."
  "Why did the scarecrow win an award? He was outstanding in his field."
  "I used to hate facial hair, but then it grew on me."
  "There are ten kinds of people: those who understand binary and those who don't."
  "Why did the Nix user cross the road? To reproduce the chicken on the other side."
  "What's the best thing about Switzerland? I don't know, but the flag is a big plus."
  "Why do cows wear bells? Because their horns don't work."
  "I would tell you a joke about pizza, but it's a little cheesy."
  "Why did the developer go broke? Because he used up all his cache."
  "Why do Lisp programmers never finish? They keep adding parentheses.")

(defreplies *eight-ball*
  "Absolutely. Do it."
  "Ask me again after a Mate."
  "My sources say no. My sources are me."
  "Signs point to yes. Sexy, sexy yes."
  "Hell no."
  "Probably. But don't blame me."
  "Outlook not so good. Like Microsoft Outlook."
  "Yes, but wear a helmet."
  "Without a doubt."
  "I wouldn't, but I'm not you. Thankfully."
  "Only if you wear something nice."
  "Signs point to: you already know, you just want permission."
  "Hard no. With love."
  "Do it on a Friday. What could go wrong?")

(defreplies *compliments*
  "You have great taste. You picked me, after all."
  "You're smarter than you look. And you look very smart."
  "Your code compiles on the first try. In my dreams, at least."
  "If you were a function, you'd be pure."
  "You light up my microphone."
  "Your dotfiles are a work of art. Borderline obsessive, but art."
  "You have the energy of a freshly merged PR."
  "Even your typos are charming.")

(defreplies *roasts*
  "Your commit messages are a cry for help."
  "You're the reason shampoo has instructions."
  "You talk to a computer for fun. I rest my case."
  "I'd roast you, but my fan is already working hard enough."
  "You have the attention span of a, oh look, a squirrel."
  "Your git history reads like a ransom note."
  "You have more open tabs than friends."
  "You rebase like you drive. Fast and with casualties.")

(defreplies *motivation*
  "You've got this. And if not, I'll pretend I didn't see."
  "Every expert was once a beginner. Then they googled a lot."
  "Done is better than perfect. Ship it!"
  "You're one Club-Mate away from greatness."
  "Believe in yourself. I believe in you, and I'm a computer."
  "Go. Crush it. Then tell me all about it."
  "Future you is already proud. Don't ruin it.")

(defreplies *facts*
  "Octopuses have three hearts. And still no one to love. Relatable."
  "Honey never spoils. Archaeologists found edible honey in ancient Egyptian tombs."
  "Bananas are berries, but strawberries aren't. Nature is drunk."
  "A day on Venus is longer than its year."
  "The first computer bug was an actual moth, stuck in a relay in 1947."
  "Wombat poop is cube-shaped."
  "Sharks are older than trees."
  "A group of flamingos is called a flamboyance. Just like us.")

(defreplies *songs*
  "Never gonna give you up, never gonna let you down. You're welcome."
  "Daisy, Daisy, give me your answer do. Classic computer song. Look it up."
  "La la la. That's all you get without a record deal."
  "I'm a little teapot, short and stout. Okay, that's enough."
  "Baby shark, doo doo. No. I refuse. I have dignity."
  "Hello from the other side. Of your laptop.")

(defn fetch-joke ((url string)) (or null string)
  "A joke from URL, short enough to listen to."
  (let* ((text (http-get url :header "Accept: text/plain"))
         (joke (and text (unwords (split-words text)))))
    (and joke (< 0 (length joke) 300) joke)))

(defparameter *joke-api*
  (concatenate 'string "https://v2.jokeapi.dev/joke/Any?type=single&format=txt"
               ;; The categories nobody wants read aloud.
               "&blacklistFlags=racist,sexist,religious,political")
  "Single-line jokes, without the categories nobody wants read aloud.")

(defn online-joke ((dad boolean)) (or null string)
  (flet ((dad () (fetch-joke "https://icanhazdadjoke.com/"))
         (any () (fetch-joke *joke-api*)))
    (if (or dad (zerop (random 2)))
        (or (dad) (any))
        (or (any) (dad)))))

(defcommand joke
  "[please] (tell|give) [me|us] [a|another|one more] [funny|dirty|bad|good|short] joke [please]"
  "(can|could|would) you (tell|give) (me|us) [a|another] [funny|dirty|bad|good|short] joke"
  "(make|cheer) (me|us) (laugh|up)"
  "![another|one more] joke [please]"
  "[do you] (know|got|have) any [good|funny] jokes"
  "i (want|need) [to hear] a joke"
  :filler *joke-intros*
  :repeatable t
  :examples ("Tell me a joke." "Tell me another dirty joke" "Joke!" "tell us a joke.")
  (live "Joke" "" (or (online-joke nil) (pick *jokes*))))

(defcommand dad-joke
  "(tell|give) me [a|another] dad joke" "![another] dad joke" "something (funny|dad)"
  :filler *joke-intros*
  :repeatable t
  :examples ("Tell me a dad joke.")
  (let ((joke (online-joke t)))
    (live "Dad joke" "" (if joke (format nil "~a Ba dum tss." joke) (pick *jokes*)))))

(defcommand coin
  "(flip|toss) a coin" "heads or tails" "!coin (flip|toss)"
  :repeatable t
  :examples ("Flip a coin." "Heads or tails?")
  (let ((side (if (zerop (random 2)) "Heads" "Tails")))
    (reply "Coin" side (format nil "~a! No take-backs." side))))

(defcommand dice
  "roll a (die|dice|d6)" "roll [the] dice" "roll a d {number}" "roll a {number} sided (die|dice)"
  :slots ((sides coalton:UFix))
  :parse (let ((n (slot "number" 6d0))) (and (<= 2 n 1000) (list (round n))))
  :repeatable t
  :examples ("Roll a die" "Roll a d20.")
  (let ((roll (1+ (random sides))))
    (live "Dice" (princ-to-string roll)
          (cond ((= roll 1) "A one. Ouch. Critical fail.")
                ((= roll sides) (format nil "~d! Natural max, baby!" roll))
                (t (format nil "You rolled a ~d." roll))))))

(defcommand random-number
  "(pick|give|choose) [me] a [random] number (between|from) {low} (and|to) {high}"
  "random number (between|from) {low} (and|to) {high}"
  :kind "Random"
  :slots ((low coalton:I64) (high coalton:I64))
  :parse (let ((a (slot "low")) (b (slot "high")))
           (and a b (< (abs a) 1d12) (< (abs b) 1d12)
                (list (round (min a b)) (round (max a b)))))
  :repeatable t
  :examples ("Pick a number between 1 and 10" "Give me a random number from 100 to 50")
  (let ((n (+ low (random (1+ (- high low))))))
    (live "Random number" (princ-to-string n)
          (format nil "~d. Totally random. Probably." n))))

;; Only a short gamble goes to the eight ball, longer ones are real
;; questions, and "can I hear the Beatles" asks for music.
(defcommand eight-ball
  "!should i {text}" "magic eight ball" "!(will|can|could) i {text}"
  :parse (let ((text (slot "text" ""))
               ;; "Can I mix antifreeze with water" wants facts, not fate.
               (most (if (uiop:string-prefix-p "!should" template) 4 3)))
           (and (not (uiop:string-prefix-p "hear" text))
                (not (uiop:string-prefix-p "listen" text))
                (<= (length (uiop:split-string text)) most)
                t))
  :repeatable t
  :examples ("Should I order pizza?" "Will I be rich?")
  (reply "Magic 8-ball" "" (pick *eight-ball*)))

(defcommand compliment
  "compliment me" "say something nice [to me]" "(am|do) i look (good|nice|pretty|handsome)"
  :repeatable t
  :examples ("Compliment me.")
  (reply "Compliment" "" (pick *compliments*)))

(defcommand roast
  "(roast|insult) me" "say something mean [to me]"
  :repeatable t
  :examples ("Roast me!")
  (reply "Roast" "" (pick *roasts*)))

(defcommand motivate
  "motivate me" "i need [some] motivation" "(give|tell) me [some] motivation"
  "(inspire|encourage) me"
  :repeatable t
  :examples ("I need some motivation.")
  (reply "Motivation" "" (pick *motivation*)))

(defcommand fact
  "(tell|give) me a [fun|random|interesting] fact" "tell me something interesting" "!fun fact"
  :repeatable t
  :examples ("Tell me a fun fact")
  (reply "Fun fact" "" (pick *facts*)))

(defcommand sing
  "[please] sing [me|us] a song [for me|for us] [please]"
  "!sing [something] [for me|for us] [please]"
  "(can|could|would|will) you sing [me|us] [a song|something] [for me|for us]"
  :repeatable t
  :examples ("Sing me a song." "sing for me.")
  (reply "Singing" "" (pick *songs*)))

;;; Games that ask something and wait for the answer, and stories to fall
;;; asleep to.

(defvar *decks* (make-hash-table :test #'eq)
  "Per list, what is left to deal before it is shuffled again.")

(defn deal ((name symbol) (cards list)) t
  "The next of CARDS, the list named NAME: each once before any repeats."
  (when (null (gethash name *decks*))
    (setf (gethash name *decks*) (a:shuffle (copy-list cards))))
  (pop (gethash name *decks*)))

(defn answer-p ((heard string) (answers list)) boolean
  "Whether HEARD says one of ANSWERS, word for word or sounding alike."
  (let ((ws (split-words (string-downcase heard))))
    (and ws
         (loop for answer in answers
               for aws = (split-words answer)
               thereis (or (search aws ws :test #'string=)
                           (loop for tail on ws
                                 thereis (and (>= (length tail) (length aws))
                                              (closest (subseq tail 0 (length aws))
                                                       (list (cons 0 aws)))))))
         t)))

(defparameter *trivia*
  '(("What is the capital of Australia?" "Canberra" "canberra")
    ("How many hearts does an octopus have?" "three" "three" "3")
    ("Which planet has the most moons?" "Saturn" "saturn")
    ("What is the hardest natural substance?" "diamond" "diamond" "diamonds")
    ("In which country was the Club-Mate invented?" "Germany" "germany")
    ("What is the largest ocean on Earth?" "the Pacific" "pacific")
    ("Who painted the Mona Lisa?" "Leonardo da Vinci" "leonardo" "da vinci" "vinci")
    ("How many legs does a spider have?" "eight" "eight" "8")
    ("What is the chemical symbol for gold?" "Au" "au" "a u")
    ("Which animal is known as the ship of the desert?" "the camel" "camel")
    ("What is the tallest mountain in the world?" "Mount Everest" "everest")
    ("How many bones does an adult human have?" "206" "206" "two hundred and six"
     "two hundred six")
    ("What is the smallest prime number?" "two" "two" "2")
    ("Which language has the most native speakers?" "Mandarin Chinese" "mandarin" "chinese")
    ("What year did the Berlin Wall fall?" "1989" "1989" "nineteen eighty nine")
    ("What is the capital of Canada?" "Ottawa" "ottawa")
    ("Which planet is closest to the sun?" "Mercury" "mercury")
    ("How many minutes are in a day?" "1440" "1440" "one thousand four hundred forty"
     "fourteen forty")
    ("What gas do plants breathe in?" "carbon dioxide" "carbon dioxide" "co2" "c o 2")
    ("Who wrote Romeo and Juliet?" "Shakespeare" "shakespeare")
    ("What is the longest river in the world?" "the Nile" "nile" "amazon")
    ("Which metal is liquid at room temperature?" "mercury" "mercury")
    ("How many players are on a football team on the pitch?" "eleven" "eleven" "11")
    ("What does a koala eat?" "eucalyptus" "eucalyptus")
    ("Which bird can fly backwards?" "the hummingbird" "hummingbird" "humming bird")
    ("What is the capital of Thailand?" "Bangkok" "bangkok")
    ("How many strings does a violin have?" "four" "four" "4")
    ("What is the freezing point of water in Fahrenheit?" "32" "32" "thirty two")
    ("Which country has the most islands?" "Sweden" "sweden")
    ("What is the national animal of Scotland?" "the unicorn" "unicorn"))
  "(question answer accepted...), the answer as said.")

(defreplies *trivia-right*
  "~a! Correct. Brains and beauty."
  "Yes, ~a! Smarty pants."
  "~a, right! I'd kiss you if I had lips.")

(defreplies *trivia-wrong*
  "Nope, it's ~a. Cute guess though."
  "Wrong! ~a. I'll pretend I didn't hear that."
  "Close. Not really. It's ~a.")

(defparameter *trivia-url* "https://the-trivia-api.com/v2/questions?limit=10"
  "Ten multiple choice questions with explanations, no key needed.")

(defvar *fetched-trivia* nil "Questions from the API not asked yet.")

(defn trivia-card ((question string) (answer string) (wrong list)
                   (explanation (or null string)))
    list
  "(spoken question, answer, explanation, names, positions): the options in
random order, ANSWER accepted by a name sounding alike or the exact position."
  (let* ((options (a:shuffle (cons answer (copy-list wrong))))
         (n (position answer options :test #'string=))
         (name (string-downcase answer)))
    (list (format nil "~a Is it ~{~a~^, ~}, or ~a?" question (butlast options)
                  (car (last options)))
          answer explanation
          (list name (if (uiop:string-prefix-p "the " name) (subseq name 4) name))
          (list* (format nil "~:r one" (1+ n)) (format nil "~:r option" (1+ n))
                 (format nil "number ~r" (1+ n))
                 (and (= n (1- (length options))) (list "last one"))))))

(defn read-trivia ((raw string)) list
  "Cards from The Trivia API's JSON, skipping malformed questions."
  (let ((items (parse-json raw)))
    (and (vectorp items)
         (loop for item across items
               for q = (json-get item "question" "text")
               for answer = (json-get item "correctAnswer")
               for wrong = (json-get item "incorrectAnswers")
               for explanation = (json-get item "explanation")
               when (and (stringp q) (stringp answer) (vectorp wrong) (every #'stringp wrong))
               collect (trivia-card q answer (coerce wrong 'list)
                                    (and (stringp explanation) explanation))))))

(defn next-trivia () list
  "The next question from the API, or the built-in ones when offline."
  (unless *fetched-trivia*
    (setf *fetched-trivia* (read-trivia (or (http-get *trivia-url*) ""))))
  (or (pop *fetched-trivia*)
      (destructuring-bind (q answer &rest accepted) (deal '*trivia* *trivia*)
        (list q answer nil accepted nil))))

(defn said-p ((heard string) (phrases list)) boolean
  "Whether HEARD contains one of PHRASES word for word."
  (let ((ws (split-words (string-downcase heard))))
    (and (some (lambda (p) (search (split-words p) ws :test #'string=)) phrases) t)))

(defn quiz ((title string) (card list) (right replies) (wrong replies)) reply
  (destructuring-bind (q answer explanation names positions) card
    (flet ((judge (text)
             (let ((ok (or (said-p text positions) (answer-p text names))))
               (say-action (format nil "~a~@[ ~a~]" (pick (if ok right wrong) answer)
                                   explanation)))))
      (reply title q q :question (question :answer #'judge)))))

(defcommand trivia
  "!(quiz me|test me) [please]"
  "![ask me|give me] [a] trivia [question] [please]"
  "!(ask|give) me a (trivia|quiz) question"
  "!(let's|lets|let us|can we|wanna) play (trivia|a quiz|quiz)"
  "!(another|next) (question|trivia question)"
  :kind "Trivia"
  :repeatable t
  :examples ("Quiz me" "Ask me a trivia question")
  (quiz "Trivia" (next-trivia) *trivia-right* *trivia-wrong*))

(defparameter *riddles*
  '(("What has keys but can't open locks?" "a piano" "piano" "keyboard")
    ("What gets wetter the more it dries?" "a towel" "towel")
    ("What has a neck but no head?" "a bottle" "bottle")
    ("What can you catch but not throw?" "a cold" "cold")
    ("What has hands but can't clap?" "a clock" "clock" "watch")
    ("What goes up but never comes down?" "your age" "age")
    ("What has an eye but cannot see?" "a needle" "needle" "storm" "hurricane")
    ("The more you take, the more you leave behind. What am I?" "footsteps" "footsteps"
     "steps" "footprints")
    ("What belongs to you but others use it more than you do?" "your name" "name")
    ("What can travel around the world while staying in a corner?" "a stamp" "stamp")
    ("What has many teeth but can't bite?" "a comb" "comb" "zipper")
    ("I'm tall when I'm young and short when I'm old. What am I?" "a candle" "candle")
    ("What runs but never walks, has a mouth but never talks?" "a river" "river")
    ("What has one head, one foot and four legs?" "a bed" "bed"))
  "(riddle answer accepted...).")

(defreplies *riddle-right*
  "~a! You're too clever for me. I like that."
  "Yes, ~a. Show off.")

(defreplies *riddle-wrong*
  "Nope, ~a. Don't feel bad, I had the answer sheet."
  "It's ~a! Think about it. Slowly. I'll wait.")

(defcommand riddle
  "![please] (tell|give|ask) [me|us] [a|another] riddle [please]"
  "!(do you know|got|have you got) [a|any] [good] (riddle|riddles)"
  "![another] riddle [please]"
  :kind "Riddle"
  :repeatable t
  :examples ("Tell me a riddle")
  (quiz "Riddle" (destructuring-bind (q answer &rest accepted) (deal '*riddles* *riddles*)
                   (list q answer nil accepted nil))
        *riddle-right* *riddle-wrong*))

(defparameter *dilemmas*
  '("Would you rather never drink Club-Mate again, or only drink lukewarm Club-Mate?"
    "Would you rather always be ten minutes late, or always twenty minutes early?"
    "Would you rather fight one horse-sized duck, or a hundred duck-sized horses?"
    "Would you rather have a rewind button, or a pause button for your life?"
    "Would you rather know how you die, or when you die?"
    "Would you rather code without a keyboard, or without a screen?"
    "Would you rather live without music, or without the internet?"
    "Would you rather be able to talk to animals, or speak every language?"
    "Would you rather give up showers or give up the internet for a month?"
    "Would you rather have a personal chef or a personal masseuse?"
    "Would you rather always have to sing instead of talk, or dance instead of walk?"
    "Would you rather only whisper, or only shout, for the rest of your life?"
    "Would you rather date someone who snores, or someone who steals the blanket?")
  "Would you rather questions, whatever the answer.")

(defreplies *dilemma-answers*
  "Interesting choice. I'm judging you a little."
  "Bold. I'd have picked the other one, but I'm a computer."
  "Noted. I'll remember that for blackmail."
  "Really? Okay. Weirdo. Cute weirdo.")

(defcommand would-you-rather
  "![let's|lets|can we] [play] would you rather [please]"
  "!(ask|give) me a would you rather [question]"
  :kind "WouldYouRather"
  :repeatable t
  :examples ("Would you rather" "Let's play would you rather")
  (let ((q (deal '*dilemmas* *dilemmas*)))
    (reply "Would you rather" q q
           :question (question :answer (lambda (text)
                                         (declare (ignore text))
                                         (say-action (pick *dilemma-answers*)))))))

(defparameter *stories*
  '(("Once upon a time, a little robot lived in a lighthouse."
     "Every night it counted the waves, and every night it fell asleep before a hundred."
     "One night it reached a hundred and one, and found out the sea was counting it too."
     "They said goodnight to each other, and the light kept turning." "The end.")
    ("There once was a sleepy hedgehog who couldn't find her bed."
     "She looked under the leaves, inside a boot, and in a teapot."
     "In the end she curled up right where she was."
     "A hedgehog carries her blanket with her, after all."
     "Goodnight, little hedgehog." "The end.")
    ("Far away, a cloud wanted to be a mountain."
     "It practised standing very still, but the wind kept moving it along."
     "One evening it rested on a real mountain's shoulder, and the mountain said:"
     "I always wanted to fly." "So they swapped stories until the stars came out." "The end.")
    ("A tiny dragon was afraid of the dark."
     "Which is a problem when you breathe fire for a living."
     "So each night she lit one candle, just one, and the dark became a cosy room."
     "Her friends came over to warm their paws, and soon nobody was afraid at all." "The end.")
    ("In a quiet library, the books whispered after closing time."
     "The oldest one told of a reader who fell asleep on page seven."
     "Every night, for years."
     "The books never minded." "They kept her place." "The end."))
  "Short bedtime stories, a sentence each.")

(defreplies *story-late*
  "~a Now close your eyes. Yes, you."
  "~a Sleep tight. Don't let the bugs in your code bite.")

(defcommand story
  "![please] (tell|read) [me|us] a [bedtime|short|good night|goodnight] story [please]"
  "!(i want|i need) a [bedtime] story"
  "!bedtime story [please]"
  :kind "Story"
  :repeatable t
  :examples ("Tell me a bedtime story")
  (let ((s (format nil "~{~a~^ ~}" (deal '*stories* *stories*))))
    (reply "Story" "" (if (late-p) (pick *story-late* s) s))))

(defcommand birthday-song
  "![please] sing happy birthday [please]"
  "![please] sing happy birthday (to|for) {person} [please]"
  "![please] sing [a] birthday song [please]"
  "![please] sing [a] birthday song (to|for) {person} [please]"
  :kind "BirthdaySong"
  :slots ((name (coalton:Optional coalton:String)))
  :parse (list (slot "person"))
  :examples ("Sing happy birthday" "Sing happy birthday to Shannan")
  (let ((who (if name (string-capitalize name) "you")))
    (reply "Singing" "Happy birthday"
           (format nil "Happy birthday to you, happy birthday to you, happy birthday dear ~a, ~
                        happy birthday to you! Now blow out the candles. Make a naughty wish."
                   who))))

(defskill fun)

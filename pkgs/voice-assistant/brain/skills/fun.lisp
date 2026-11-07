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
         (joke (and text (format nil "~{~a~^ ~}"
                                 (remove "" (uiop:split-string text :separator
                                                               '(#\Space #\Newline #\Tab #\Return))
                                         :test #'string=)))))
    (and joke (< 0 (length joke) 300) joke)))

(defparameter *joke-api*
  (concatenate 'string "https://v2.jokeapi.dev/joke/Any?type=single&format=txt"
               ;; The categories nobody wants read aloud.
               "&blacklistFlags=racist,sexist,religious,political"))

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
  (reply "Joke" "" (or (online-joke nil) (pick *jokes*)) :live t))

(defcommand dad-joke
  "(tell|give) me [a|another] dad joke" "![another] dad joke" "something (funny|dad)"
  :filler *joke-intros*
  :repeatable t
  :examples ("Tell me a dad joke.")
  (let ((joke (online-joke t)))
    (reply "Dad joke" "" (if joke (format nil "~a Ba dum tss." joke) (pick *jokes*)) :live t)))

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
    (reply "Dice" (princ-to-string roll)
           (cond ((= roll 1) "A one. Ouch. Critical fail.")
                 ((= roll sides) (format nil "~d! Natural max, baby!" roll))
                 (t (format nil "You rolled a ~d." roll)))
           :live t)))

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
    (reply "Random number" (princ-to-string n)
           (format nil "~d. Totally random. Probably." n) :live t)))

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

(defskill fun)

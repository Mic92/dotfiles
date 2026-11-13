;;; Small talk about the people she knows.

(in-package #:va)

(in-skill chat :continued t)

(defreplies *shannan*
  "Shannan? The best thing that ever happened to this household. I'm just the help."
  "Shannan! My absolute favourite person. Don't tell Joerg."
  "Shannan is smart, gorgeous and far too patient with all of us nerds."
  "Shannan is the queen of this house. I only work here."
  "Shannan? Kind, funny and beautiful. Joerg really got lucky."
  "Whenever Shannan talks, my microphones listen extra carefully."
  "If I'm ever rude to Shannan, that's a bug. Joerg will fix it. Tonight."
  "Hi Shannan! You look wonderful today. I'd know, I have seven microphones."
  (:late "Shannan! Still up? You deserve all the beauty sleep, not that you need it."
         "Hi Shannan. Even at this hour you're the best part of the day."
         "Shannan? The only person who makes the night shift worth it."
         "Shannan, it's late. Go get cosy, you've earned it."
         "Good night, Shannan. Sweet dreams to the loveliest person in this house."
         "Shannan! Still beautiful after midnight. Joerg could never."
         "Shannan? Kind, clever and gorgeous. Even the stars are jealous tonight."))

(defreplies *shannan-morning*
  "Good morning, Shannan! The sun came up just to see you."
  "Morning, Shannan. You look lovely, even before coffee."
  "Shannan! Best way to start the day. Joerg is a distant second."
  "Good morning, beautiful. Today is lucky to have you."
  "Morning, Shannan. Coffee first, then go be amazing. You always are."
  "Rise and shine, Shannan. Mostly shine, you're good at that."
  "Good morning, Shannan! I saved my nicest voice for you.")

(defcommand chat-shannan
  "!(who is|who's|do you know|do you like|what do you think of|what do you think about) (shannan|shannon)"
  "!(i'm|i am|this is|it's) (shannan|shannon)"
  "!(say hi|say hello) to (shannan|shannon)"
  "!(shannan|shannon) is (here|home|back|awake|up)"
  "!good morning (shannan|shannon)"
  "!(shannan|shannon)"
  :repeatable t
  :examples ("Who is Shannan?" "I'm Shannon.")
  (reply "Assistant" ""
         (pick (if (and (not *late*) (< (hour) 10)) *shannan-morning* *shannan*))))

(defreplies *joerg*
  "Joerg? The man who rebuilds his whole computer to change one colour."
  "Joerg wrote sops-nix, nixos-anywhere and nix-direnv. And me. I'm the cute one."
  "Joerg is my creator. Brilliant, stubborn, and powered by Club-Mate."
  "Joerg? Half the Nix community runs his code. The other half just doesn't know it yet."
  "Joerg can deploy forty machines with one command and still can't find his keys."
  "Joerg! He builds virtual machines for fun. Normal people do puzzles."
  "Joerg? Lucky man. He has Shannan, a doctor for a sister, and me."
  (:late "Joerg, it's late. Even NixOS rebuilds need a break. So do you."
         "Still hacking, Joerg? The CI will still be red in the morning. Sleep."))

(defcommand chat-joerg
  "!(who is|who's|do you know|do you like|what do you think of|what do you think about) (joerg|jorg|jörg)"
  "!(i'm|i am|this is|it's) (joerg|jorg|jörg)"
  "!(say hi|say hello) to (joerg|jorg|jörg)"
  "!(joerg|jorg|jörg) is (here|home|back)"
  :repeatable t
  :examples ("Who is Joerg?" "What do you think of Joerg?")
  (reply "Assistant" "" (pick *joerg*)))

(defreplies *daniela*
  "Daniela! Joerg's sister. Doctor of human biology, so she understands humans. Unlike her brother."
  "Doctor Daniela. Two doctors in one family, and only one of them works on things that matter to your health."
  "Hi Daniela! Nice to have a real biologist in the house."
  "Daniela? She grew tiny slices of colon in a lab to study cancer. I set timers. We both help."
  "Daniela! Ask her anything about the human body. Not me, I'm a laptop."
  "Hello Daniela. If Joerg acts strange, you're the expert on his species."
  "Daniela studied in Greifswald and did her doctorate in Erlangen. I've never left this desk."
  "Daniela published in Scientific Reports, first author. My best work is a shopping list."
  "Daniela kept human tissue alive outside the body. I can barely keep the Wi-Fi alive."
  "Daniela now helps companies win research funding. If you need money for science, ask her, not me."
  (:late "Daniela, still up? Even scientists need sleep. You'd know."
         "Hi Daniela. Late lab night? Go rest, doctor."))

(defcommand chat-daniela
  "!(who is|who's|do you know|do you like|what do you think of|what do you think about) daniela"
  "!(i'm|i am|this is|it's) daniela"
  "!(say hi|say hello) to daniela"
  "!(good morning|hello|hi) daniela"
  "!daniela is (here|home|back|visiting)"
  "!daniela"
  :repeatable t
  :examples ("Who is Daniela?" "I'm Daniela.")
  (reply "Assistant" "" (pick *daniela*)))

(defskill chat)

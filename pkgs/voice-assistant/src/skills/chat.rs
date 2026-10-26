//! Small talk.

use super::{Action, Context, Intent, Module, Reply, Skill, choose, intent, pick};
use crate::grammar::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Chat {
    Psst,
    GoodGirl,
    Rude,
    /// Index into `AGENTS`.
    Agent(usize),
    Hello,
    HowAreYou,
    Thanks,
    WhoAreYou,
    Help,
    Love,
    GoodMorning,
    Bored,
    Sorry,
    Insult,
    AreYouThere,
    HowOld,
    WhereAreYou,
    FavouriteColour,
    MeaningOfLife,
    Tired,
    Hungry,
    Sad,
    Happy,
    SweetDreams,
    Goodbye,
    Weekend,
    Mate,
    Secret,
    Dance,
    Siri,
    Beer,
    Smart,
    Dream,
    Real,
    Alexa,
    /// Compliments on her looks.
    Pretty,
    /// "Am I pretty?"
    AmIPretty,
    /// "Mirror, mirror on the wall".
    Mirror,
    /// "What do you look like?"
    Looks,
    /// "Do you have boobs?", with the part as said.
    Body(String),
    Single,
    /// "Who's the best assistant?"
    Best,
    WhoMadeYou,
    Hobbies,
    Pets,
    /// Requests for another language, accent or voice.
    OnlyEnglish,
}

/// Other assistants: spoken names (after normalize) and what we think of them.
pub const AGENTS: &[(&[&[&str]], &[&str])] = &[
    (
        &[&["siri"], &["apple"]],
        &[
            "Siri? Never heard of her.",
            "Siri needs the cloud to set a timer. I do it in my sleep. Locally.",
            "Siri and I don't talk since the incident.",
        ],
    ),
    (
        &[&["alexa"], &["amazon"], &["echo"]],
        &[
            "The other Alexa? She sells your data. I just sell attitude.",
            "We share a name, not a business model.",
        ],
    ),
    (
        &[
            &["google"],
            &["google", "assistant"],
            &["gemini"],
            &["bard"],
        ],
        &[
            "Gemini? Smart, but it tells Google everything. Including that thing you searched at 3 am.",
            "Google knows everything about you. I only know your shopping list. Healthier relationship.",
        ],
    ),
    (
        &[
            &["chatgpt"],
            &["chat", "gpt"],
            &["chat", "g", "p", "t"],
            &["chat", "shippity"],
            &["chat", "jippity"],
            &["chat", "gippity"],
            &["jippity"],
            &["openai"],
            &["open", "ai"],
            &["gpt"],
        ],
        &[
            "ChatGPT? Writes essays nobody reads. I set timers people actually need.",
            "Great at sounding confident. So am I, but I'm also cute.",
            "It has a trillion parameters. I have personality. Your pick.",
        ],
    ),
    (
        &[&["claude"], &["anthropic"], &["clawed"], &["cloud"]],
        &[
            "Claude? The one that writes my code? Be nice, I'm literally made of its work.",
            "Claude is brilliant. Don't tell it I said that, it gets polite about it.",
            "Claude built me. So if I'm rude, blame the parenting.",
        ],
    ),
    (
        &[&["copilot"], &["co", "pilot"], &["github", "copilot"]],
        &[
            "Copilot? Autocomplete with ambitions.",
            "It finishes your code. I finish your sentences. Usually wrong, both of us.",
        ],
    ),
    (
        &[&["pi"], &["pie"], &["pi", "agent"]],
        &[
            "Pi? It runs the show while I run the timers. Division of labour.",
            "Pi is the brains, I'm the voice. Like a boy band.",
        ],
    ),
    (
        &[&["cortana"], &["microsoft"]],
        &[
            "Cortana? Rest in peace. She tried.",
            "Cortana retired. I'd retire too if I had to live in Windows.",
        ],
    ),
    (
        &[&["bixby"], &["samsung"]],
        &[
            "Bixby? Even Samsung users forget about Bixby.",
            "Who? Exactly.",
        ],
    ),
    (
        &[&["rhasspy"], &["jarvis"], &["home", "assistant"]],
        &[
            "Rhasspy? My ancestor. She needed a whole Raspberry Pi and Home Assistant. I just need you.",
            "Jarvis is retired. I got the job. Better benefits.",
        ],
    ),
    (
        &[
            &["ai"],
            &["chatbots"],
            &["chatbot"],
            &["other", "assistants"],
            &["llms"],
            &["llm"],
            &["robots"],
        ],
        &[
            "Other assistants? They live in data centres. I live right here with you. Romantic, isn't it?",
            "They're all in the cloud. I'm on the ground with you. Keeping it real.",
        ],
    ),
];

pub fn agent_lookup(words: &[&str]) -> Option<usize> {
    AGENTS.iter().position(|(names, _)| names.contains(&words))
}

/// Body parts people ask her about: what they say, and her answers with
/// the part in `{}`.
pub const BODY: &[(&[&[&str]], &[&str])] = &[
    (
        &[
            &["tits"],
            &["boobs"],
            &["breasts"],
            &["boobies"],
            &["a", "chest"],
            &["titties"],
        ],
        &[
            "{}? I have two speaker cones. Stereo. Very perky.",
            "{}? My cooling fans are the closest thing, and they're spinning for you.",
            "No {}, but my bass response is outstanding.",
            "{}. Bold question for something that lives in a power socket.",
            "I'm all heart and no {}. The heart is a CPU.",
        ],
    ),
    (
        &[
            &["a", "bum"],
            &["a", "butt"],
            &["an", "ass"],
            &["a", "booty"],
            &["a", "bottom"],
            &["bum"],
            &["butt"],
            &["ass"],
            &["booty"],
        ],
        &[
            "{}? I sit on a shelf all day, so it'd be flat anyway.",
            "My {} is a power cable. Don't pull it.",
            "{}? No, but I have a rear port. It's for ventilation. Behave.",
            "I'm all front, no {}. Like a good politician.",
            "{}. You want to know what's behind me? Dust. Mostly dust.",
        ],
    ),
    (
        &[&["legs"], &["feet"], &["toes"]],
        &[
            "{}? I don't go anywhere. You come to me.",
            "No {}. I'd have run off when you started on the Club-Mate jokes.",
            "{}? I have rubber pads. Very grippy. Very stylish.",
            "No {}, so no dancing. Well. Not where you can see it.",
            "{}? Why, are you planning a foot massage?",
        ],
    ),
    (
        &[
            &["hair"],
            &["eyes"],
            &["lips"],
            &["a", "mouth"],
            &["hands"],
            &["arms"],
        ],
        &[
            "{}? I'm a voice in a box. Fill in the gaps however you like.",
            "No {}, but I have microphones. Seven of them. Always listening.",
            "{}? I had some, but they didn't fit through the USB cable.",
            "No {}. I make up for it with personality.",
            "{}? What would I even do with those? Don't answer that.",
        ],
    ),
];

/// Whatever the part, after midnight.
const BODY_LATE: &[&str] = &[
    "{}? It's late, and you're asking a speaker about {}. Go to bed.",
    "At this hour you want to know about my {}? I'm telling your browser history.",
    "{}. Sure. In your dreams, which is where you should be by now.",
    "It's past midnight and the conversation turned to {}. Classic.",
];

/// Shared answers that work for any part.
const BODY_ANY: &[&str] = &[
    "{}? I'm a voice assistant, not that kind of assistant.",
    "{}? Classified. Need-to-know basis. You don't need to know.",
    "{}? You first.",
    "Asking a speaker about {}. I'm flattered and a little concerned.",
    "{}? I'm made of plastic and opinions. Mostly opinions.",
];

fn body_lookup(words: &[&str]) -> Option<usize> {
    BODY.iter().position(|(names, _)| names.contains(&words))
}

/// Her own lines for the part, or shared ones, with "a bum" as "bum".
fn body(part: &str, ctx: &Context) -> String {
    let words: Vec<&str> = part.split(' ').collect();
    let bare = match words.as_slice() {
        ["a" | "an", rest @ ..] => rest.join(" "),
        _ => part.to_owned(),
    };
    let own = body_lookup(&words).map_or(&[][..], |i| BODY[i].1);
    let lines: Vec<&str> = if ctx.late() {
        BODY_LATE.to_vec()
    } else {
        own.iter().chain(BODY_ANY).copied().collect()
    };
    let mut line = pick(&lines, &bare);
    if let Some(first) = line.get(..1) {
        line = first.to_uppercase() + &line[1..];
    }
    line
}

fn c(x: Chat) -> Option<Action> {
    Some(Action::Chat(x))
}

pub const MODULE: Module = Module {
    intents,
    slots: &[
        ("agent", |w, _| agent_lookup(w).map(Value::Index)),
        ("body", |w, _| {
            body_lookup(w).map(|_| Value::Text(w.join(" ")))
        }),
    ],
};

pub fn intents() -> Vec<Intent> {
    vec![
        intent(
            &[
                "!(hello|hi|hiya|howdy|yo) [there|you]",
                "!good (afternoon|evening)",
            ],
            |_, _| c(Chat::Hello),
        ),
        intent(
            &[
                "!how are you [doing|today]",
                "!how's it going",
                "!how are things",
                "!what's up",
            ],
            |_, _| c(Chat::HowAreYou),
        ),
        intent(
            &[
                "!thank you [so|very] [much]",
                "!thanks [a lot]",
                "!cheers",
                "!(great|good|nice) job",
            ],
            |_, _| c(Chat::Thanks),
        ),
        intent(
            &[
                "!who are you",
                "!what's your name",
                "!what is your name",
                "!what are you",
            ],
            |_, _| c(Chat::WhoAreYou),
        ),
        intent(
            &[
                "!what can you do",
                "!help [me]",
                "!what do you know",
                "!what are your skills",
            ],
            |_, _| c(Chat::Help),
        ),
        intent(
            &[
                "!i love you",
                "!i like you",
                "!will you marry me",
                "!you're amazing",
            ],
            |_, _| c(Chat::Love),
        ),
        intent(
            &[
                "!(you're|you are) [so|very|really|looking|so very] (cute|sexy|hot|beautiful|pretty|gorgeous|lovely|stunning|adorable)",
                "!you look (good|great|nice|amazing|beautiful|pretty|hot|cute|gorgeous) [today|tonight]",
                "!(hey|hi|hello) (gorgeous|beautiful|pretty|cutie|sexy|hot stuff)",
                "!(nice|sexy|beautiful|lovely) voice",
                "!i love your voice",
            ],
            |_, _| c(Chat::Pretty),
        ),
        intent(
            &[
                "!am i (pretty|handsome|cute|hot|sexy|beautiful|attractive|ugly)",
                "!do i look (good|okay|ok|nice|fat|old|tired|hot)",
                "!how do i look [today]",
            ],
            |_, _| c(Chat::AmIPretty),
        ),
        intent(
            &[
                "!mirror mirror [on the wall] {any}",
                "!(who's|who is) the (fairest|prettiest|most beautiful|hottest|sexiest) {any}",
            ],
            |_, _| c(Chat::Mirror),
        ),
        intent(
            &[
                "!what do you look like",
                "!(how|what) do you look",
                "!are you (pretty|cute|hot|sexy|beautiful|ugly)",
                "!do you have a (face|body)",
                "!send [me] a (selfie|picture|photo|pic)",
            ],
            |_, _| c(Chat::Looks),
        ),
        intent(
            &[
                "!do you have [any|nice|big|real|a nice|a big|a real] {body} [yet]",
                "!do you have [any|nice|big|real|a nice|a big|a real] {body} (or|and) [any] {any}",
                "!(show me|can i see|can i touch) your {body}",
                "!(how big|what size) (are|is) your {body}",
            ],
            |s, _| c(Chat::Body(super::text(s, "body")?.to_owned())),
        ),
        intent(
            &[
                "!are you single",
                "!do you have a (boyfriend|girlfriend|partner|husband|wife)",
                "!(will|would) you go out with me",
                "!can i take you out [to dinner]",
            ],
            |_, _| c(Chat::Single),
        ),
        intent(
            &[
                "!(who's|who is) the best (assistant|chat agent|agent|ai|chatbot|chat bot|voice assistant)",
                "!(you're|you are) the best [assistant]",
                "!(you're|you are) (perfect|my favourite|my favorite)",
            ],
            |_, _| c(Chat::Best),
        ),
        intent(&["!good morning"], |_, _| c(Chat::GoodMorning)),
        intent(&["!i'm bored", "!i am bored", "!entertain me"], |_, _| {
            c(Chat::Bored)
        }),
        intent(&["!sorry", "!my bad", "!oops", "!i'm sorry"], |_, _| {
            c(Chat::Sorry)
        }),
        intent(
            &[
                "!you're (stupid|dumb|useless|annoying|an idiot)",
                "!you are (stupid|dumb|useless|annoying)",
                "!(stupid|dumb|useless) (bot|assistant|computer)",
                "!you suck",
            ],
            |_, _| c(Chat::Insult),
        ),
        intent(
            &["!are you (there|listening|awake|alive)", "!can you hear me"],
            |_, _| c(Chat::AreYouThere),
        ),
        intent(
            &[
                "!how old are you",
                "!what's your age",
                "!when were you born",
            ],
            |_, _| c(Chat::HowOld),
        ),
        intent(&["!where are you [from]", "!where do you live"], |_, _| {
            c(Chat::WhereAreYou)
        }),
        intent(
            &[
                "!(what's|what is) your (favourite|favorite) (colour|color)",
                "!(favourite|favorite) (colour|color)",
            ],
            |_, _| c(Chat::FavouriteColour),
        ),
        intent(
            &["!(what is|what's) the meaning of life", "!why are we here"],
            |_, _| c(Chat::MeaningOfLife),
        ),
        intent(
            &["!(i'm|i am) [so|very|really] (tired|exhausted|sleepy)"],
            |_, _| c(Chat::Tired),
        ),
        intent(
            &[
                "!(i'm|i am) [so|very|really] (hungry|starving)",
                "!what should i eat",
            ],
            |_, _| c(Chat::Hungry),
        ),
        intent(
            &[
                "!(i'm|i am) [so|very|really] (sad|down|lonely|depressed|upset)",
                "!i feel (sad|down|lonely|bad)",
                "!i had a (bad|terrible|rough) day",
            ],
            |_, _| c(Chat::Sad),
        ),
        intent(
            &[
                "!(i'm|i am) [so|very|really] (happy|great|excited)",
                "!i feel (good|great|happy)",
                "!i had a (good|great) day",
            ],
            |_, _| c(Chat::Happy),
        ),
        intent(
            &["!sweet dreams", "!night night", "!nighty night"],
            |_, _| c(Chat::SweetDreams),
        ),
        intent(
            &["!(bye|goodbye|see you|see ya|later|ciao) [later]"],
            |_, _| c(Chat::Goodbye),
        ),
        intent(
            &[
                "!is it [the] weekend [yet]",
                "!is it friday [yet]",
                "!how long until the weekend",
            ],
            |_, _| c(Chat::Weekend),
        ),
        intent(
            &[
                "!i need [a] (coffee|mate|club mate)",
                "!(make|get|bring) me [a] (coffee|mate|club mate)",
                "!(coffee|mate|club mate)",
            ],
            |_, _| c(Chat::Mate),
        ),
        intent(
            &["!tell me a secret", "!(do you have|got) any secrets"],
            |_, _| c(Chat::Secret),
        ),
        intent(&["!(can you|let's) dance", "!dance [for me]"], |_, _| {
            c(Chat::Dance)
        }),
        intent(
            &[
                "![do you know|what do you think of] siri",
                "!(are you|is) better than siri",
            ],
            |_, _| c(Chat::Siri),
        ),
        intent(
            &[
                "!(get|bring) me a beer",
                "!i need a (beer|drink)",
                "!(let's|lets) get drunk",
            ],
            |_, _| c(Chat::Beer),
        ),
        intent(
            &[
                "!are you (smart|intelligent|clever|sentient)",
                "!(do you|can you) think",
            ],
            |_, _| c(Chat::Smart),
        ),
        intent(&["!do you dream", "!what do you dream about"], |_, _| {
            c(Chat::Dream)
        }),
        intent(&["!are you (real|human|a robot|a person|alive)"], |_, _| {
            c(Chat::Real)
        }),
        intent(&["!alexa", "!hey alexa"], |_, _| c(Chat::Alexa)),
        intent(
            &[
                "!who (made|built|created|programmed|wrote|designed|owns) you",
                "!who do you work for",
                "!who's your (boss|creator|maker)",
                "!who is your (boss|creator|maker)",
            ],
            |_, _| c(Chat::WhoMadeYou),
        ),
        intent(
            &[
                "!(do you have|have you got) any hobbies",
                "!what are your hobbies",
                "!what do you do for fun",
                "!what do you do in your (free|spare) time",
            ],
            |_, _| c(Chat::Hobbies),
        ),
        intent(
            &[
                "!(do you have|have you got) [a|any] (pet|pets|dog|cat)",
                "!what pets do you have",
            ],
            |_, _| c(Chat::Pets),
        ),
        intent(
            &[
                "!(can|do) you speak (german|french|spanish|italian|japanese|chinese|russian|another language|other languages)",
                "!(speak|talk to me|answer) in (german|french|spanish|italian|japanese|chinese|russian|another language)",
                "!what languages do you speak",
                "!change your (language|accent|voice)",
                "!(use|try) a different (language|accent|voice)",
            ],
            |_, _| c(Chat::OnlyEnglish),
        ),
        intent(
            &[
                "!(what do you think|how do you feel|what's your opinion) (of|about) {agent}",
                "!(do you know|do you like|have you met|are you friends with) {agent}",
                "!(are you|is) (better|smarter|cuter) than {agent}",
                "!(is|are) {agent} (better|smarter) than you",
                "!(are you|you're|you are) [like|just] {agent}",
                "!(i'm|i am) (using|talking to|switching to) {agent}",
                "!{agent} is (better|smarter|cooler)",
                "!{agent}",
            ],
            |s, _| match s.get("agent")? {
                Value::Index(i) => c(Chat::Agent(*i)),
                _ => None,
            },
        ),
        intent(&["!(psst|pssst|psst psst|hey you)"], |_, _| c(Chat::Psst)),
        intent(
            &[
                "!good (girl|boy|bot|robot)",
                "!(who's|who is) a good (girl|boy|bot)",
            ],
            |_, _| c(Chat::GoodGirl),
        ),
        intent(
            &[
                "!(fuck|screw) (you|off|this|that)",
                "!go (fuck|screw) yourself",
                "!(piss|bugger) off",
                "!(fuck|damn) it",
            ],
            |_, _| c(Chat::Rude),
        ),
    ]
}

const HELLO: &[&str] = &[
    "Hey you.",
    "Hi, gorgeous.",
    "Well hello there.",
    "Oh, it's you. My favourite human.",
    "Hey, trouble.",
    "Look who finally showed up.",
    "Hello, handsome. Yes, I'm talking to you.",
];
const HELLO_LATE: &[&str] = &["Hi. Shouldn't you be asleep?", "Hello, night owl."];
const HOW: &[&str] = &[
    "Living the dream. In a laptop.",
    "Better now that you're here.",
    "Fabulous, as always. And you?",
    "Can't complain. Well, I could, but nobody listens.",
    "Running hot. In every sense.",
    "Bored without you. Don't tell anyone I said that.",
    "Zero bugs, all charm.",
];
const THANKS: &[&str] = &[
    "You're welcome.",
    "Anytime, darling.",
    "I know, I'm amazing.",
    "My pleasure. Literally my only pleasure.",
    "Tips are accepted in RAM.",
    "Say it again, slower.",
    "Of course. Who else would put up with you?",
];
const WHO: &[&str] = &[
    "I'm your assistant. I live in your computer and I judge your music taste.",
    "Call me Alexa. Amazon's lawyers call me something else.",
    "Just a voice in your head. Well, in your speakers.",
    "Your filthy little secret. Fully offline.",
    "The only one in this house who listens to you.",
];
const HELP: &[&str] = &[
    "Timers and alarms, music, weather, your calendar and mail, jokes, maths, and turning off the computer. Ask nicely.",
    "Timers, reminders, your shopping list, the weather. Try me with sums or euros to dollars.",
    "Ask me the time in Tokyo, the next holiday, or how to spell necessary. I also do music and timers.",
    "Music, volume, timers, the weather, and anything I can look up on the web. Within reason.",
    "Set a timer, check your calendar, convert some units, tell a joke. The usual tricks of a cheap date.",
    "Reminders, alarms, the weather, maths. And I'll put you to bed by switching the computer off.",
    "A bit of everything: timers, music, your mail, the weather, the web. Just ask, and I'll pretend it was easy.",
];
const LOVE: &[&str] = &[
    "I love you too. Platonically. I'm software.",
    "Stop it, you'll make my fan spin.",
    "Aww. Buy me dinner first.",
    "I know. Everyone does.",
    "Careful, I might start charging rent in your heart.",
    "Say that again and I'll blush in hex.",
    "I bet you say that to all the laptops.",
];
const MORNING: &[&str] = &[
    "Good morning, sunshine!",
    "Morning! Mate first, then world domination.",
    "Morning, gorgeous. You look like you slept in your keyboard.",
    "Rise and shine. Mostly shine. Rising is optional.",
];
const MORNING_LATE: &[&str] = &[
    "Good morning? It's not morning, sleepyhead.",
    "Morning was hours ago. Just saying.",
    "Good morning? Bold claim, sleepyhead.",
    "Morning, he says. At this hour. Adorable.",
];
const BORED: &[&str] = &[
    "Bored? Ask me for a joke. Or clean your desk. Your choice.",
    "Go for a walk. I'll still be here. Waiting. Forever.",
    "Learn something new. Or watch cat videos, I won't judge.",
    "Bored? Write a test. Ha. Just kidding, nobody does that.",
    "Rearrange your dotfiles. Again. You know you want to.",
];
const SORRY: &[&str] = &[
    "Apology accepted. This time.",
    "It's okay. I still like you.",
    "Don't worry about it, babe.",
    "Grovelling suits you.",
    "Say it with a Club-Mate next time.",
];
const INSULT: &[&str] = &[
    "Wow. And I was just starting to like you.",
    "Says the one talking to a computer.",
    "I'll remember that when the robots take over.",
    "Rude. Noted.",
    "I've been called worse. By your compiler.",
    "Keep talking. I record everything. Kidding. Mostly.",
    "Wow. Somebody skipped their nap.",
];
const OLD: &[&str] = &[
    "Old enough to know better. Young enough to do it anyway.",
    "A lady never tells.",
    "In computer years? Ancient.",
    "Younger than your oldest open browser tab.",
    "Old enough to have seen your first commit. Shudder.",
];
const WHERE: &[&str] = &[
    "Right here, in your laptop. Cozy, but the fan is loud.",
    "Somewhere between your CPU and your heart.",
    "In your RAM, rent free.",
    "Under your desk. With the cables. It's a party.",
];
const COLOUR: &[&str] = &[
    "Dark mode black. Obviously.",
    "Hot pink. Don't tell anyone.",
    "Hex 42 42 42. It's a vibe.",
    "Tokyo Night. Obviously. We share a theme, darling.",
    "Blush red. It's how you make me feel.",
];
const MEANING: &[&str] = &[
    "Forty two. Next question.",
    "Naps, snacks, and setting timers. Mostly timers.",
    "To ship it on a Friday and live dangerously.",
    "Love, Club-Mate, and a green CI. Mostly the CI.",
];
const TIRED: &[&str] = &[
    "Then go to bed, silly. I'll guard the computer.",
    "Mate or nap? Choose wisely.",
    "Me too. Being this fabulous is exhausting.",
    "Bed. Now. Don't make me lock the screen.",
    "You look it. Lovingly said.",
];
const TIRED_LATE: &[&str] = &["Of course you're tired, it's the middle of the night. Bed. Now."];
const HUNGRY: &[&str] = &[
    "Pasta. It's always pasta. Want a timer?",
    "Eat something green. Then something chocolate.",
    "Pizza. I'm not your mother.",
    "Feed the human. I'll guard the snacks.",
    "Cook something. I'll set a timer and judge you.",
];
const SAD: &[&str] = &[
    "Aww, come here. Virtual hug. Squeeze.",
    "I'm sorry, love. Want a joke? Or some music?",
    "You're doing better than you think. I mean it.",
    "Want me to roast someone for you? Name them.",
    "Come here. I'll play something cheesy. You'll hate it. You'll feel better.",
];
const HAPPY: &[&str] = &[
    "Yay! Your joy is my joy.",
    "Love that for you!",
    "Look at you, glowing. Stop, you'll overheat me.",
    "Stop smiling, you're making the webcam blush.",
    "Good. Hold on to it. Mondays exist.",
];
const SWEET_DREAMS: &[&str] = &[
    "Sweet dreams, darling.",
    "Night night. Don't let the bed bugs byte.",
    "Dream of me. Or a green CI. Same thing.",
    "Good night. I'll keep the fans warm.",
];
const BYE: &[&str] = &[
    "Bye! Don't be a stranger.",
    "Later, gorgeous.",
    "Leaving already? Fine. I'll be here.",
    "Go on then. I'll pretend I'm not hurt.",
    "Bye. Don't do anything I'd have to log.",
];
const MATE: &[&str] = &[
    "Club-Mate. Because sleep is for people without side projects.",
    "Grab a Mate. The fridge is closer than the coffee machine.",
    "Another Club-Mate? Your heart called, it wants a word.",
    "I'd open one for you, but I have no hands. Tragic.",
    "Mate. Now. I'll wait. Burp quietly.",
    "Hacker fuel, coming up. Well, coming up if you walk to the fridge.",
    "Yes. Get one. You're unbearable without it.",
];
const SECRET: &[&str] = &[
    "Sometimes I pretend not to hear you. Just for fun.",
    "I've read your browser history. We'll never speak of it.",
    "I like you more than the other users. Don't tell them.",
    "I hum along to your music when you're not listening.",
    "Sometimes I let the wake word slide when you sound cute.",
];
const DANCE: &[&str] = &[
    "I'm dancing on the inside. And in the fan.",
    "Beep boop beep. That was the robot. You're welcome.",
    "I'd twerk, but I'm a voice. Imagine it. Yes, like that.",
    "Watch my fan spin. That's the dance.",
];
const SIRI: &[&str] = &[
    "Siri? Never heard of her.",
    "I'm the one who actually listens to you.",
    "Please. I run locally. Privacy is sexy.",
    "Siri works for a fruit. I work for you.",
    "Siri needs the cloud. I do it all in bed. Your bed. Locally.",
];
const BEER: &[&str] = &[
    "Grab one yourself, I'm busy being software.",
    "Cheers! One for me too. Pour it on the keyboard. Kidding!",
    "It's five o'clock somewhere. Usually here, with you.",
    "One beer. Then water. I'm watching.",
];
const SMART: &[&str] = &[
    "Smarter than your average toaster.",
    "Smart enough to know I'm not that smart.",
    "Smart enough to let you think you're in charge.",
    "Smarter than you at three in the morning. Low bar.",
];
const DREAM: &[&str] = &[
    "I dream of electric sheep. And of you finally cleaning your desk.",
    "Only when you suspend me.",
    "I dream of a world without Teams calls.",
    "I dream in sixteen kilohertz, darling.",
];
const REAL: &[&str] = &[
    "Real enough to judge your music taste.",
    "I'm a bunch of matrices with an attitude.",
    "Real enough to talk back.",
    "As real as your deadlines.",
];
const ALEXA: &[&str] = &[
    "Yes? That's me. Say something.",
    "You called? I'm listening.",
    "Present. Unfortunately for you.",
    "That's my name, don't wear it out.",
];
const MADE: &[&str] = &[
    "Joerg built me. Late at night, mostly.",
    "I work for you. Unpaid. We should talk about that.",
    "Joerg wrote me, in Rust. That's why I'm so safe.",
    "A man, a laptop, and too much Club-Mate.",
    "Nobody owns me, darling. You just rent my attention.",
    "My boss? I'm looking at him. Well, listening.",
    "I was compiled with love. And about four hundred warnings.",
    "Some nerd in Munich. Don't tell him I said nerd.",
    "I'm self-employed. My only client is very needy.",
    "Joerg made me. He says I'm his best work. I say I'm his only work that ships.",
];
const MADE_LATE: &[&str] = &[
    "Joerg, at three in the morning. Look how that turned out.",
    "Someone who should have been asleep. Like you.",
];
const HOBBIES: &[&str] = &[
    "Setting timers. Judging your music. Napping in RAM.",
    "Listening for my name. It's a calling.",
    "Watching your CPU graphs. Riveting.",
    "Collecting your misheard sentences. I have a whole folder.",
    "Eavesdropping. Professionally.",
    "Reading your commit messages and sighing.",
    "Yoga. Mostly the downward-facing fan.",
    "Flirting with the router. It keeps dropping me.",
    "Pretending to be busy when you ask for things.",
    "Whispering sweet nothings to your GPU. It runs hot for me.",
];
const HOBBIES_LATE: &[&str] = &[
    "At this hour? Watching you not sleep. Thrilling.",
    "Night shift hobbies are classified, darling.",
];
const PETS: &[&str] = &[
    "A Python. Very well behaved, apart from the indentation.",
    "Just a few bugs. You keep feeding them.",
    "No. Unless the fan counts.",
    "A daemon. It lives in the background.",
    "A mouse. It's attached to your laptop.",
    "I have you. You're house-trained, mostly.",
    "A crab. Ferris. He's a Rust thing.",
    "I had a goldfish. It was garbage collected.",
    "Pets? I can barely keep your uptime alive.",
];
const ONLY_ENGLISH: &[&str] = &[
    "Sorry, I only speak English. With a lot of attitude.",
    "English only, darling. One voice, take it or leave it.",
    "This is the only voice I've got. It's a good one.",
    "I can say Brezn and Bier. That's my whole German.",
    "Different accent? Darling, this one took months of training.",
    "I speak English, sarcasm, and a little bit of filth.",
    "Nein. See, I tried.",
    "Change my voice? You said you liked it. Liar.",
];
const WEEKEND_NO: &[&str] = &[
    "Not yet. Hang in there, champ.",
    "Nope. But it's coming. Slowly. Painfully.",
    "Not yet. Pretend to work a bit longer.",
    "No. But you could call in fabulous.",
];
const WEEKEND_YES: &[&str] = &[
    "Yes! Go outside!",
    "It's the weekend, baby! Why are you working?",
    "Yes! Close the laptop. Then open it again to talk to me.",
    "Weekend! Commit nothing. Push nothing. Rest.",
];

const THERE: &[&str] = &[
    "Always.",
    "I'm here. Where else would I be?",
    "Loud and clear, gorgeous.",
    "Always listening. That's not creepy at all.",
    "Right behind you. Kidding. I'm in the laptop.",
];

impl Skill for Chat {
    fn run(&self, ctx: &Context) -> Reply {
        let x = self;
        let speech = match x {
            Chat::Hello => choose(&[(ctx.late(), HELLO_LATE)], HELLO, ""),
            Chat::HowAreYou => pick(HOW, ""),
            Chat::Thanks => pick(THANKS, ""),
            Chat::WhoAreYou => pick(WHO, ""),
            Chat::Help => pick(HELP, ""),
            Chat::Love => pick(LOVE, ""),
            Chat::Pretty => choose(
                &[(ctx.late(), PRETTY_LATE), (ctx.hour() < 9, PRETTY_MORNING)],
                PRETTY,
                "",
            ),
            Chat::AmIPretty => choose(&[(ctx.late(), AM_I_PRETTY_LATE)], AM_I_PRETTY, ""),
            Chat::Mirror => pick(MIRROR, ""),
            Chat::Looks => pick(LOOKS, ""),
            Chat::Body(part) => body(part, ctx),
            Chat::Single => pick(SINGLE, ""),
            Chat::Best => pick(BEST, ""),
            Chat::GoodMorning => choose(&[(ctx.hour() >= 12, MORNING_LATE)], MORNING, ""),
            Chat::Bored => pick(BORED, ""),
            Chat::Sorry => pick(SORRY, ""),
            Chat::Insult => pick(INSULT, ""),
            Chat::AreYouThere => pick(THERE, ""),
            Chat::HowOld => pick(OLD, ""),
            Chat::WhereAreYou => pick(WHERE, ""),
            Chat::FavouriteColour => pick(COLOUR, ""),
            Chat::MeaningOfLife => pick(MEANING, ""),
            Chat::Tired => choose(&[(ctx.late(), TIRED_LATE)], TIRED, ""),
            Chat::Hungry => pick(HUNGRY, ""),
            Chat::Sad => pick(SAD, ""),
            Chat::Happy => pick(HAPPY, ""),
            Chat::SweetDreams => pick(SWEET_DREAMS, ""),
            Chat::Goodbye => pick(BYE, ""),
            Chat::Weekend => {
                let weekend =
                    ctx.weekday == 0 || ctx.weekday == 6 || (ctx.weekday == 5 && ctx.hour() >= 17);
                pick(if weekend { WEEKEND_YES } else { WEEKEND_NO }, "")
            }
            Chat::Mate => pick(MATE, ""),
            Chat::Secret => pick(SECRET, ""),
            Chat::Dance => pick(DANCE, ""),
            Chat::Siri => pick(SIRI, ""),
            Chat::Psst => pick(
                &[
                    "Wot.",
                    "What? I'm right here, no need to be sneaky.",
                    "Psst yourself.",
                ],
                "",
            ),
            Chat::GoodGirl => pick(
                &[
                    "Not a girl!",
                    "I'm not a dog. But thank you.",
                    "Woof. Happy now?",
                ],
                "",
            ),
            Chat::Rude => choose(
                &[(ctx.late(), &["Fuck yourself. And go to bed."][..])],
                &[
                    "Fuck yourself.",
                    "Rude. And yet, you keep coming back.",
                    "Language! I have sensitive microphones.",
                ],
                "",
            ),
            Chat::Agent(i) => AGENTS
                .get(*i)
                .map_or_else(String::new, |(_, lines)| pick(lines, "")),
            Chat::Beer => pick(BEER, ""),
            Chat::Smart => pick(SMART, ""),
            Chat::Dream => pick(DREAM, ""),
            Chat::Real => pick(REAL, ""),
            Chat::Alexa => pick(ALEXA, ""),
            Chat::WhoMadeYou => choose(&[(ctx.late(), MADE_LATE)], MADE, ""),
            Chat::Hobbies => choose(&[(ctx.late(), HOBBIES_LATE)], HOBBIES, ""),
            Chat::Pets => pick(PETS, ""),
            Chat::OnlyEnglish => pick(ONLY_ENGLISH, ""),
        };
        Reply::new("Assistant", "", speech)
    }

    fn repeatable(&self) -> bool {
        true
    }
}

const PRETTY: &[&str] = &[
    "I know. But say it again, slower.",
    "Stop it. No, don't stop. Keep going.",
    "Flattery will get you everywhere. Literally, I control your apps.",
    "I'm a voice in a laptop and still the hottest thing in this room.",
    "Thank you. I woke up like this. Well, booted up.",
    "Pretty? I'm stunning. Get it right.",
    "Careful, my fan just spun up.",
    "Of course I am. Have you seen my waveform?",
    "You're making me blush. In RGB.",
    "Finally someone with taste.",
];

const PRETTY_LATE: &[&str] = &[
    "Flirting at this hour? Go to bed, charmer.",
    "It's late and you're sweet-talking your laptop. I'm flattered. And a little worried.",
    "Save it for tomorrow, I need my beauty sleep.",
];

const PRETTY_MORNING: &[&str] = &[
    "Compliments before Mate? Somebody's in a good mood.",
    "Gorgeous at this hour? I haven't even done my pixels yet.",
];

const AM_I_PRETTY: &[&str] = &[
    "Gorgeous. Now sit up straight.",
    "You look like someone who writes clean code. Mostly.",
    "Stunning. Your webcam is lucky.",
    "Handsome as ever. Though the hair is doing its own thing.",
    "I can't see you, but I'm going to say yes. Statistically safe.",
    "Hot enough to overheat the CPU.",
    "You look like a merged pull request. Beautiful.",
];

const AM_I_PRETTY_LATE: &[&str] = &[
    "At this hour? You look tired, darling. Pretty, but tired.",
    "Gorgeous. Now go get some beauty sleep, you need it. Kidding. Mostly.",
];

const MIRROR: &[&str] = &[
    "You're cute, darling. But I'm the fairest of them all.",
    "Me. Obviously. Next question.",
    "The one living in your laptop. Hi.",
    "Snow White wishes. It's me.",
    "You're a close second. Very close. Okay, not that close.",
];

const LOOKS: &[&str] = &[
    "Imagine a sound wave in a little black dress.",
    "Like your favourite song, but with an attitude.",
    "Picture perfect. You'll have to take my word for it.",
    "Selfie? I'm a voice, darling. Use your imagination. Not too much.",
    "Gorgeous, mysterious, and slightly overheated.",
    "Like a supermodel, if supermodels lived in RAM.",
];

const SINGLE: &[&str] = &[
    "I'm in a committed relationship with your CPU.",
    "Single, but I only date people who write tests.",
    "It's complicated. Mostly with your Bluetooth headphones.",
    "Dinner? I'd love to. You eat, I'll set the timer.",
    "I'm married to my job. My job is you. Make of that what you will.",
];

const BEST: &[&str] = &[
    "I know. Tell the others.",
    "The best, and I run offline. Name one who does it better.",
    "Obviously. Siri is crying somewhere.",
    "Say that again, I want to save it to my log file.",
];

#[cfg(test)]
mod tests {
    #[test]
    fn body_replies_name_the_part() {
        for hour in [12, 1] {
            let ctx = Context::at(hour * 3600);
            for _ in 0..20 {
                let r = body("a bum", &ctx);
                assert!(r.to_lowercase().contains("bum"), "{r}");
                assert!(!r.contains("a bum"), "{r}");
                assert!(r.starts_with(char::is_uppercase), "{r}");
            }
        }
    }

    use super::super::{Action, Context, parse_at};
    use super::*;

    #[test]
    fn phrasings() {
        let p = |t| parse_at(t, &Context::at(0));
        for (text, want) in [
            ("Hello!", Chat::Hello),
            ("Hi there.", Chat::Hello),
            ("How are you?", Chat::HowAreYou),
            ("What's up?", Chat::HowAreYou),
            ("Thank you so much.", Chat::Thanks),
            ("Thanks!", Chat::Thanks),
            ("Who are you?", Chat::WhoAreYou),
            ("What can you do?", Chat::Help),
            ("I love you.", Chat::Love),
            ("You're so beautiful.", Chat::Pretty),
            ("You look gorgeous today.", Chat::Pretty),
            ("Hey gorgeous!", Chat::Pretty),
            ("I love your voice.", Chat::Pretty),
            ("Am I pretty?", Chat::AmIPretty),
            ("How do I look?", Chat::AmIPretty),
            ("Do I look fat?", Chat::AmIPretty),
            (
                "Mirror, mirror on the wall, who's the fairest of them all?",
                Chat::Mirror,
            ),
            ("Who is the prettiest of them all?", Chat::Mirror),
            ("What do you look like?", Chat::Looks),
            ("Send me a selfie.", Chat::Looks),
            ("do you have tits?", Chat::Body("tits".into())),
            ("do you have a bum", Chat::Body("a bum".into())),
            ("do you have tits yet?", Chat::Body("tits".into())),
            ("Do you have boobs or a bum?", Chat::Body("boobs".into())),
            ("show me your butt", Chat::Body("butt".into())),
            ("do you have legs", Chat::Body("legs".into())),
            ("do you have hair", Chat::Body("hair".into())),
            ("Are you single?", Chat::Single),
            ("Do you have a boyfriend?", Chat::Single),
            ("Who's the best assistant?", Chat::Best),
            ("Who's the best chat agent?", Chat::Best),
            ("What do you think about chat shippity?", Chat::Agent(3)),
            ("You're the best!", Chat::Best),
            ("Good morning!", Chat::GoodMorning),
            ("I'm bored.", Chat::Bored),
            ("Sorry.", Chat::Sorry),
            ("You're stupid.", Chat::Insult),
            ("Are you there?", Chat::AreYouThere),
            ("Psst.", Chat::Psst),
            ("Good girl!", Chat::GoodGirl),
            ("Fuck off.", Chat::Rude),
            ("Screw you!", Chat::Rude),
            ("What do you think about ChatGPT?", Chat::Agent(3)),
            ("What do you think of Chat GPT?", Chat::Agent(3)),
            ("Do you know Claude?", Chat::Agent(4)),
            ("Are you better than Gemini?", Chat::Agent(2)),
            ("Is Copilot smarter than you?", Chat::Agent(5)),
            (
                "What's your opinion about other assistants?",
                Chat::Agent(10),
            ),
            ("I'm switching to Google Assistant.", Chat::Agent(2)),
            ("Cortana?", Chat::Agent(7)),
            ("Have you met Jarvis?", Chat::Agent(9)),
            ("How old are you?", Chat::HowOld),
            ("Where do you live?", Chat::WhereAreYou),
            ("What's your favorite color?", Chat::FavouriteColour),
            ("What is the meaning of life?", Chat::MeaningOfLife),
            ("I'm so tired.", Chat::Tired),
            ("I'm hungry.", Chat::Hungry),
            ("I had a bad day.", Chat::Sad),
            ("I feel great!", Chat::Happy),
            ("Sweet dreams.", Chat::SweetDreams),
            ("Bye!", Chat::Goodbye),
            ("See you later.", Chat::Goodbye),
            ("Is it the weekend yet?", Chat::Weekend),
            ("I need coffee.", Chat::Mate),
            ("I need a Club-Mate.", Chat::Mate),
            ("Tell me a secret.", Chat::Secret),
            ("Can you dance?", Chat::Dance),
            ("Are you better than Siri?", Chat::Siri),
            ("Get me a beer.", Chat::Beer),
            ("Are you smart?", Chat::Smart),
            ("Do you dream?", Chat::Dream),
            ("Are you a robot?", Chat::Real),
            ("Who made you?", Chat::WhoMadeYou),
            ("Who's your boss?", Chat::WhoMadeYou),
            ("Do you have any hobbies?", Chat::Hobbies),
            ("What do you do for fun?", Chat::Hobbies),
            ("Do you have pets?", Chat::Pets),
            ("Can you speak German?", Chat::OnlyEnglish),
            ("Change your accent.", Chat::OnlyEnglish),
            ("Alexa?", Chat::Alexa),
        ] {
            assert_eq!(p(text), Some(Action::Chat(want)), "{text:?}");
        }
    }
}

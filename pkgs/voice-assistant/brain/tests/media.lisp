;;; Media and fuzzy names, ported from the Rust tests.

(in-package #:va-tests)

(define-test sound-alikes
  :parent va-tests
  (let ((names '((0 "lassulus") (1 "fm4") (2 "deutschlandfunk") (3 "bayern" "3"))))
    (loop for (heard want) in '((("laslos") 0) (("lasso" "lose") 0) (("fm") 1)
                                (("deutschland" "funk") 2) (("bayern" "drei") nil)
                                (("pizza") nil) (("a") nil))
          do (is eql want (va::closest heard names) "~s" heard)))
  (false (va::closest '("yif") '((0 "eve") (1 "eva") (2 "evo") (3 "blob" "64"))))
  (false (va::closest '("ava") '((0 "eve") (1 "eva"))))
  (is = 2 (va::edit-distance (va::skeleton "kitten") (va::skeleton "sitting"))))

(define-test media-search
  :parent va-tests
  (with-focus ()
    (parses-as "Media(Search(~s))"
               '(("Play Hey Jude" "hey jude")
                 ("Play Hey Jude by the Beatles." "hey jude by the beatles")
                 ("play music by bob dylan" "bob dylan")
                 ("Play some songs by Daft Punk" "daft punk")
                 ("put my workout playlist on" "workout playlist")
                 ("I want to listen to Bohemian Rhapsody" "bohemian rhapsody")
                 ("can you play some jazz" "jazz")
                 ("can i hear some music by fleet foxes" "fleet foxes")
                 ("can i get some music by three six mafia" "three six mafia")
                 ("let's hear my workout playlist" "workout playlist")
                 ("turn my drake playlist on" "drake playlist")
                 ("turn on the rap playlist" "rap playlist")
                 ("resume my death metal playlist" "death metal playlist")
                 ("let me hear the song sympathy for the devil" "sympathy for the devil")
                 ("play the song god's plan" "god's plan")
                 ("i want to listen to my maroon 5 playlist, play it for me" "maroon 5 playlist")))
    (parses-not "Media(Search"
                '("Play the next song" "play the music" "play the news" "Play." "play it again"
                  "play a song for me" "turn on the radio" "turn on the night light"
                  "can i get the weather" "can we get a pizza" "can i hear you"
                  "start my playlist" "let me hear you say it" "i need to hear my todo list"
                  "i want to hear my to do list read out loud"))))

(define-test media-video
  :parent va-tests
  (with-focus ()
    (parses-as "Media(Video(~s))"
               '(("search youtube for cat videos" "cat videos")
                 ("Search YouTube for how to tie a tie." "how to tie a tie")
                 ("search on youtube for lofi" "lofi")
                 ("search for kurzgesagt on youtube" "kurzgesagt")
                 ("look up kurzgesagt on youtube" "kurzgesagt")
                 ("find a video about black holes" "black holes")
                 ("youtube never gonna give you up" "never gonna give you up")
                 ("play hey jude on youtube" "hey jude") ("put on hey jude on youtube" "hey jude")
                 ("show me a video of cute puppies" "cute puppies")
                 ("show me videos about sourdough" "sourdough")
                 ("play a video about the moon landing" "the moon landing")
                 ("play the video for bad guy" "bad guy")
                 ("play the music video for thriller" "thriller")
                 ("watch the trailer for dune on youtube" "the trailer for dune")
                 ("I want to watch a video about rust" "rust")
                 ("can you play a youtube video about nix" "nix")
                 ("let's watch some cat videos" "cat videos")
                 ("can i watch the dune trailer" "the dune trailer")
                 ("open youtube and search for linus tech tips" "linus tech tips")))
    (parses-not "Media(Video"
                '("search for cat videos" "play hey jude" "play the next video"))))

(define-test media-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Media(~a)"
               '(("Play the music." "Play") ("Resume the podcast" "Play") ("Play." "Play")
                 ("Pause the music." "Pause") ("Stop the music!" "Stop") ("Next song." "Next")
                 ("Skip this track" "Next") ("Next." "Next") ("Previous track" "Previous")
                 ("Play the last song again" "Previous") ("Play BBC Radio 4" "Radio(1)")
                 ("Put on BBC Radio One." "Radio(0)") ("Play six music" "Radio(2)")
                 ("Play the news" "Radio(3)") ("Turn on the radio." "Radio(3)")
                 ("Play Deutschlandfunk" "Radio(4)") ("Play FM4" "Radio(6)")
                 ("Play something chill." "Radio(9)") ("Play some lofi" "Radio(10)")
                 ("Play lassulus radio" "Radio(11)") ("Play Laslos Radio." "Radio(11)")
                 ("Play some FM radio." "Radio(6)") ("Play music" "Play")
                 ("Can I hear some music?" "Play") ("I want to listen to music" "Play")
                 ("Put my music on, please" "Play") ("Put on lassulus" "Radio(11)")
                 ("I want to listen to Bayern 3" "Radio(7)") ("Turn off the radio." "RadioOff")
                 ("What's playing?" "NowPlaying") ("What song is this?" "NowPlaying")
                 ("Who sings this?" "NowPlaying")))))

(define-test media-refines-radio
  :parent va-tests
  (let ((radio (va::command-action 'va::media-radio 3)))
    (with-focus (:previous radio)
      (is equal "Media(Radio(6))" (parsed "and fm4?")))))

(defun ambient-of (text)
  (let ((a (va::parse-text text)))
    (and a (va::named-p a 'va::media-ambient)
         (destructuring-bind (sound seconds) (va::action-fields a)
           (list (first (va::ambient-at sound)) seconds)))))

(define-test ambient-sounds
  :parent va-tests
  (with-focus ()
    (loop for (text . want) in
          '(("play rain sounds" "rain" 0) ("play some white noise" "white noise" 0)
            ("play ocean waves for 30 minutes" "ocean" 1800)
            ("put on brown noise for an hour" "brown noise" 3600)
            ("i want to hear some rain" "rain" 0) ("play the fan" "fan" 0)
            ("help me sleep" "rain" 3600) ("help me fall asleep" "rain" 3600)
            ("sleep sounds for 20 minutes" "rain" 1200))
          do (is equal want (ambient-of text) "~s" text))
    (is equal "Media(Radio(3))" (parsed "play the radio")))
  (let ((effects (va:fake-effects (va::perform (va::command-action 'va::media-ambient 0 1800)))))
    (true (find-if (lambda (e) (and (equal (first e) "timeout") (equal (second e) "1800")
                                    (find "mpv" e :test #'equal)
                                    (some (lambda (a) (search "anoisesrc" a)) e)))
                   effects)
          "~s" effects)))

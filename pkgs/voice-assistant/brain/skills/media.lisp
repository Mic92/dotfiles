;;; Media players (MPRIS) via noctalia, and our own radio and YouTube via mpv.

(in-package #:va)

(in-skill media)

(defreplies *play*
  "Music, maestro!"
  "Let's get this party started."
  "Ooh, I love this one."
  "Play it loud. The neighbours love you."
  "Hit it!")

(defreplies *pause*
  "Paused. The silence is deafening."
  "Holding the beat."
  "Music paused. Dramatic pause."
  "Paused. Go on, take that call.")

(defreplies *next*
  "Skip! Didn't like that one either."
  "Next. You're picky today."
  "Moving on."
  "Next. That song was a crime anyway."
  "Skipping. Your taste is evolving. Slowly.")

(defreplies *previous*
  "Going back. Nostalgic, are we?"
  "One more time, with feeling."
  "Rewind!"
  "Back we go. Clearly a banger.")

(defreplies *stop*
  "Music stopped. Party's over."
  "Stopped. Who killed the vibe? You did."
  "Silence. Finally, I can hear myself think.")

(defreplies *video*
  "~a, on YouTube. Grab the popcorn."
  "Searching YouTube for ~a. Skip the ads, if you can."
  "~a. Don't forget to like and subscribe. Kidding."
  "Pulling up ~a. Down the rabbit hole we go.")

(defreplies *search*
  "~a, coming right up."
  "Looking for ~a. Give me a second."
  "~a. Excellent choice. Mostly."
  "Finding ~a. Warm up those vocal cords."
  "~a? Fine. No judgement. Some judgement."
  "Digging up ~a. Dance like nobody's listening. I am."
  "~a. Turn it up, darling."
  "One ~a, served hot."
  "~a. The neighbours will love it."
  "~a. Try not to sing along. Please.")

(defreplies *radio*
  "~a, coming up. Turn it up, darling."
  "Tuning in to ~a. Very retro of you."
  "~a. Like it's 1985 and I'm a kitchen radio."
  "~a. Don't make me sing along."
  "~a, on air. Act natural.")

(defreplies *radio-news*
  "~a. Brace yourself for the state of the world."
  "~a. Stay informed, stay grumpy."
  "~a. The world, in all its chaos.")

(defreplies *radio-off*
  "Radio off. Back to the voices in your head."
  "Radio's off. Silence, finally."
  "Radio off. Just you and me now.")

(defreplies *no-radio*
  "There's no radio playing. Hearing things?"
  "Nothing to turn off. I'm the only one talking."
  "No radio playing. That's just your tinnitus.")

(defreplies *lassulus*
  "lassulus radio. Krebs-certified bangers incoming."
  "Tuning in to lassulus. Say hi to the crabs."
  "lassulus radio. Questionable taste, impeccable uptime.")

(defreplies *nothing-playing*
  "Nothing is playing. Want some music?"
  "Silence. Beautiful, isn't it?")

(defreplies *playing-paused*
  "~a, but it's paused."
  "Paused on ~a. Waiting for you.")

(defreplies *playing*
  "This is ~a."
  "~a. Good taste. Mostly."
  "You're listening to ~a. Bop along.")

#+mallet
(declaim (mallet:suppress-next :line-length))
(defparameter *stations*
  '(((("radio" "1") ("radio" "one") ("bbc" "radio" "1") ("bbc" "radio" "one") ("bbc" "1")
      ("bbc" "one"))
     "BBC Radio 1"
     "http://as-hls-ww-live.akamaized.net/pool_01505109/live/ww/bbc_radio_one/bbc_radio_one.isml/bbc_radio_one-audio%3d96000.norewind.m3u8")
    ((("radio" "4") ("radio" "four") ("bbc" "radio" "4") ("bbc" "radio" "four") ("bbc" "4")
      ("bbc" "four"))
     "BBC Radio 4"
     "http://as-hls-ww-live.akamaized.net/pool_55057080/live/ww/bbc_radio_fourfm/bbc_radio_fourfm.isml/bbc_radio_fourfm-audio%3d96000.norewind.m3u8")
    ((("6" "music") ("six" "music") ("bbc" "6" "music") ("bbc" "six" "music"))
     "BBC 6 Music"
     "http://as-hls-ww-live.akamaized.net/pool_81827798/live/ww/bbc_6music/bbc_6music.isml/bbc_6music-audio%3d96000.norewind.m3u8")
    ((("world" "service") ("bbc" "world" "service") ("bbc") ("bbc" "radio") ("the" "news")
      ("news"))
     "BBC World Service"
     "http://as-hls-ww-live.akamaized.net/pool_87948813/live/ww/bbc_world_service/bbc_world_service.isml/bbc_world_service-audio%3d96000.norewind.m3u8")
    ((("deutschlandfunk") ("deutschland" "funk") ("dlf") ("german" "radio") ("german" "news"))
     "Deutschlandfunk"
     "https://st01.sslstream.dlf.de/dlf/01/128/mp3/stream.mp3")
    ((("deutschlandfunk" "kultur") ("deutschland" "funk" "kultur") ("dlf" "kultur"))
     "Deutschlandfunk Kultur"
     "https://st02.sslstream.dlf.de/dlf/02/128/mp3/stream.mp3")
    ((("fm4") ("fm" "4") ("fm" "four"))
     "FM4"
     "https://orf-live.ors-shoutcast.at/fm4-q2a")
    ((("bayern" "3") ("bayern" "three") ("br3") ("br" "3"))
     "Bayern 3"
     "https://dispatcher.rndfnk.com/br/br3/live/mp3/mid")
    ((("ego" "fm") ("egofm") ("ego"))
     "egoFM"
     "https://streams.egofm.de/egoFM-hq")
    ((("groove" "salad") ("soma" "fm") ("somafm") ("chill" "music") ("something" "chill")
      ("chill"))
     "SomaFM Groove Salad"
     "https://ice1.somafm.com/groovesalad-128-mp3")
    ((("lofi") ("lo" "fi") ("low" "fi") ("focus" "music") ("music" "to" "focus")
      ("something" "to" "focus") ("fluid"))
     "SomaFM Fluid"
     "https://ice1.somafm.com/fluid-128-mp3")
    ((("lassulus") ("lassulus" "radio") ("lassulus's" "radio") ("lasso" "lose") ("lassul" "us")
      ("lass" "radio") ("krebs" "radio"))
     "lassulus radio"
     "https://radio.lassul.us/radio.ogg"))
  "Spoken names, display name and stream URL.")

(defn station-lookup ((ws words)) (or null (integer 0))
  "The station named WS. Whisper spells unusual names creatively, so guess
when \"radio\" was said, but not otherwise: \"turn on wifi\" is no lofi."
  (or (position-if (lambda (s) (member ws (first s) :test #'equal)) *stations*)
      (let ((heard (butlast ws)))
        (and heard (in (car (last ws)) "radio" "station")
             (closest heard (loop for (names) in *stations* for i from 0
                                  append (mapcar (lambda (n) (cons i n)) names)))))))

(defslot "station" (ws next) (station-lookup ws))
(closed-slot "station" (lambda () (mapcan (lambda (s) (copy-list (first s))) *stations*)))

(defparameter *vague*
  '("it" "that" "this" "something" "anything" "music" "radio" "again" "it again" "song" "songs"
    "playlist")
  "Objects that are no title: \"play it\", \"play a song for me\".")

(defn search-query ((text string)) (or null string)
  "\"some songs by daft punk\" -> \"daft punk\". Nil for words that are no
search: \"my todo list\" is for the notes, \"the last song again\" is a
skip, \"can i hear you\" no title."
  (let* ((ws (search-words (split-words (remove #\, text))))
         (q (unwords ws)))
    (and (not (member "list" ws :test #'string=))
         (not (in (first ws) "next" "previous" "last" "same" "you"))
         (plusp (length q))
         (not (member q *vague* :test #'string=))
         q)))

(defn search-words ((ws words)) words
  "WS without fillers around the title: \"some songs by\", \"please\", \"for me\"."
  ;; "maroon 5 playlist play it for me"
  (let ((again (position "play" ws :test #'string= :start 1)))
    (when again (setf ws (subseq ws 0 again))))
  (loop while (in (first ws) "some" "my" "the" "a" "me") do (pop ws))
  (let ((by (position "by" ws :test #'string=)))
    (when (and by (plusp by)
               (every (lambda (w) (in w "music" "songs" "song" "something" "stuff" "tracks"))
                      (subseq ws 0 by)))
      (setf ws (nthcdr (1+ by) ws))))
  (loop (cond ((in (car (last ws)) "please" "in" "here" "now") (setf ws (butlast ws)))
              ((and (rest ws) (string= (car (last ws 2)) "for") (in (car (last ws)) "me" "us"))
               (setf ws (butlast ws 2)))
              (t (return))))
  (when (and (rest ws) (in (first ws) "song" "track" "album")) (pop ws))
  ws)

(defparameter *radio-tag* "voice-assistant-radio"
  "Marks our mpv in the process list so it can be stopped again.")

(defn radio-socket () (or null string)
  (let ((dir (uiop:getenv "XDG_RUNTIME_DIR")))
    (and dir (format nil "~a/voice-assistant-radio.sock" dir))))

(defn mpv-ipc ((command string)) (or null json)
  "One request over mpv's JSON IPC, and its parsed reply."
  (let ((path (radio-socket)))
    (when (and path (not *fake-effects*))
      (handler-case
          (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
            (unwind-protect
                 (progn
                   (sb-bsd-sockets:socket-connect sock path)
                   (let ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                               :timeout 0.3d0)))
                     (format s "{\"command\": ~a, \"request_id\": 1}~%" command)
                     (finish-output s)
                     ;; Event lines may come first.
                     (loop repeat 20
                           for line = (read-line s nil)
                           while line
                           for reply = (parse-json line)
                           when (eql (json-get reply "request_id") 1) return reply)))
              (sb-bsd-sockets:socket-close sock)))
        (error () nil)))))

(defn radio-pause ((pause boolean)) boolean
  "Pause or resume our radio. When pausing, whether it was playing."
  (if pause
      (let ((state (mpv-ipc "[\"get_property\", \"pause\"]")))
        (and (hash-table-p state) (nth-value 1 (gethash "data" state))
             (not (gethash "data" state))
             (mpv-ipc "[\"set_property\", \"pause\", true]") t))
      (and (mpv-ipc "[\"set_property\", \"pause\", false]") t)))

(defn stop-radio () boolean (and (run "pkill" "-f" *radio-tag*) t))

(defn mpv ((url string) &rest (flags string)) null
  (stop-radio)
  (let ((ipc (radio-socket)))
    (apply #'spawn "mpv" "--really-quiet"
           (append flags (and ipc (list (format nil "--input-ipc-server=~a" ipc)))
                   (list url)))))

(defn noctalia-media ((command string) (lines replies)) reply
  (spawn "noctalia" "msg" "media" command)
  (reply "Media" "" (pick lines)))

(defmacro defmedia (name command lines templates &rest keys)
  `(defcommand ,name ,@templates ,@keys (noctalia-media ,command ,lines)))

(defcommand media-play
  "(play|resume|continue|start) [the|my|this|some] (music|song|track|video|podcast|media|playback)"
  "!play" "!play something"
  "!(can|could|may) i (hear|listen to) [some|my] music"
  "!(i want to|i'd like to|let's) (hear|listen to) [some|my] music"
  "!put [on] [some|my] music [on] [please]"
  :examples ("Play the music." "Can I hear some music?")
  (radio-pause nil)
  (noctalia-media "play" *play*))

(defmedia media-pause "pause" *pause*
    ("pause [the|my|this|some] (music|song|track|video|podcast|media|playback)")
  :examples ("Pause the music."))

(defcommand media-stop
  "stop [the|my|this|some] (music|song|track|video|podcast|media|playback)"
  :examples ("Stop the music!")
  (stop-radio)
  (noctalia-media "stop" *stop*))

(defmedia media-next "next" *next*
    ("[play] [the] next (song|track|one|video|episode)" "skip [this|the] [song|track|one]"
                                                        "!next")
  :repeatable t
  :examples ("Next song." "Skip this track"))

(defmedia media-previous "previous" *previous*
    ("[play] [the] (previous|last) (song|track|one|video|episode)"
     "go back [a|one] (song|track)"
     "!previous")
  :repeatable t
  :examples ("Previous track" "Play the last song again"))

(defn radio ((i (integer 0))) reply
  (destructuring-bind (&optional names name url) (nth i *stations*)
    (declare (ignore names))
    (cond
      ((null name) (reply "Radio" "" "I don't know that station."))
      (t
       (mpv url "--no-video" (format nil "--force-media-title=~a" *radio-tag*))
       (reply "Radio" name
              (cond ((uiop:string-prefix-p "lassulus" name) (pick *lassulus*))
                    ((or (search "World" name) (search "Radio 4" name)
                         (uiop:string-prefix-p "Deutschlandfunk" name))
                     (pick *radio-news* name))
                    (t (pick *radio* name))))))))

(defn refine-radio ((previous action) (ws words)) (or null action)
  "\"and bbc radio 1?\""
  (declare (ignore previous))
  (let ((i (station-lookup ws))) (and i (command-action 'media-radio i))))

(defparameter *ambient*
  '((("rain" "rainfall" "rain sounds" "the rain" "raindrops") "rain"
     "anoisesrc=c=pink:a=0.2,highpass=f=400,lowpass=f=5000")
    (("white noise" "white") "white noise" "anoisesrc=c=white:a=0.08")
    (("pink noise" "pink") "pink noise" "anoisesrc=c=pink:a=0.15")
    (("brown noise" "brown" "brownian noise") "brown noise" "anoisesrc=c=brown:a=0.3")
    (("ocean" "ocean waves" "waves" "the sea" "sea" "the ocean" "sea waves") "ocean"
     "anoisesrc=c=brown:a=0.4,lowpass=f=1500,tremolo=f=0.1:d=0.8")
    (("fan" "the fan" "fan noise") "fan" "anoisesrc=c=brown:a=0.35,lowpass=f=400"))
  "Spoken names, the name, and the ffmpeg filter mpv makes the sound with.")

(defn ambient-at ((i (integer 0))) list (rest (nth i *ambient*)))

(defslot "sound" (ws next)
  (let ((spoken (unwords (if (member (car (last ws)) '("sounds" "sound" "noises") :test #'string=)
                             (butlast ws)
                             ws))))
    (position-if (lambda (s) (member spoken (first s) :test #'string=)) *ambient*)))

(defreplies *ambient-on*
  "~a~a. Sweet dreams."
  "~a~a. Close your eyes, I've got this."
  "~a~a. Shh. Relax."
  (:late "~a~a. Night night, sleepyhead."
         "~a~a. Finally, bedtime. I was getting worried."))

(defcommand media-ambient
  "!(play|put on|start|turn on) [some|the] {sound} [please]"
  "!(play|put on|start|turn on) [some|the] {sound} for {duration}"
  "!(i want|i'd like|i want to|i'd like to) [hear|listen to] [some|the] {sound} [please]"
  "!(i want|i'd like|i want to|i'd like to) [hear|listen to] [some|the] {sound} for {duration}"
  "!help me (sleep|fall asleep|get to sleep|relax|calm down) [please]"
  "!(sleep|relaxing|calming|calm|background) (sounds|noise) [please]"
  "!(sleep|relaxing|calming|calm|background) (sounds|noise) for {duration}"
  :kind "Ambient"
  :slots ((sound coalton:UFix) (seconds coalton:UFix))
  :parse (let ((sound (slot "sound")) (secs (slot "duration")))
           (list (or sound 0) (or secs (if sound 0 3600))))
  :examples ("Play rain sounds" "Help me sleep")
  (destructuring-bind (name filter) (ambient-at sound)
    (stop-radio)
    (let ((player (append (list "mpv" "--no-video" "--really-quiet"
                                (format nil "--force-media-title=~a" *radio-tag*))
                          (and (radio-socket)
                               (list (format nil "--input-ipc-server=~a" (radio-socket))))
                          (list (format nil "av://lavfi:~a" filter)))))
      (if (plusp seconds)
          (apply #'spawn "timeout" (princ-to-string seconds) player)
          (apply #'spawn player)))
    (reply "Ambient" name
           (pick *ambient-on* (string-capitalize name :end 1)
                 (if (plusp seconds) (format nil " for ~a" (spoken-duration seconds)) "")))))

(defn youtube ((hits (integer 1)) (query string) &rest (flags string)) null
  "Play the first HITS YouTube results for QUERY. Fresh stream URLs answer
403 for the first few seconds, hence the reconnects."
  (apply #'mpv (format nil "ytdl://ytsearch~d:~a" hits query)
         "--stream-lavf-o=reconnect=1,reconnect_on_http_error=403,reconnect_delay_max=10"
         (format nil "--title=~a" *radio-tag*) flags))

(defcommand media-radio
  (:join "!(play|put on|turn on|start|listen to|switch to|tune in to|tune into) [the|some]"
         "{station} [radio] [please]")
  "!(play|put on|turn on|start) [the|some] radio {station}"
  "!i want to (listen to|hear) [the|some] {station}"
  :kind "Radio"
  :slots ((station coalton:UFix))
  :refine #'refine-radio
  :examples ("Play BBC Radio 4" "Play some lofi")
  (radio station))

;; The rhasspy days played the BBC by default.
(defcommand media-radio-default
  "!(play|put on|turn on|start) [the|some] radio" "!radio [please]"
  :kind "Radio"
  :slots ((station coalton:UFix))
  :parse (list 3)
  :refine #'refine-radio
  :examples ("Turn on the radio.")
  (radio station))

(defcommand media-radio-off
  "!(stop|turn off|switch off|kill|enough) [the|that] radio" "!radio off"
  :examples ("Turn off the radio.")
  (reply "Radio" "" (pick (if (stop-radio) *radio-off* *no-radio*))))

;; Before the plain "play {text}", which would take "... on youtube".
(defcommand media-video
  "!(search|look up|find) [on] youtube [for] {text}"
  "!(search|look up|find) [for] {text} on youtube"
  "!open youtube and (search|look up) [for] {text}"
  "!youtube {text}"
  "!(play|put on|watch|show me) {text} on youtube"
  (:join "!(play|put on|watch|show|find) [me] [a|the|some] [youtube|music] (video|videos)"
         "(about|of|for|on|with) {text}")
  (:join "!(i want to|i'd like to|let's|can i|can we) watch [a|some] [youtube] (video|videos)"
         "(about|of|for|on|with) {text}")
  "!(i want to|i'd like to|let's|can i|can we) watch [some] {text}"
  (:join "!(can you|could you) (play|show me|find) [me] [a|the|some] [youtube|music]"
         "(video|videos) (about|of|for|on|with) {text}")
  :kind "Video"
  :slots ((query coalton:String))
  :parse (list (slot "text"))
  :examples ("search youtube for cat videos" "show me a video of cute puppies")
  (youtube 1 query)
  (reply "YouTube" query (pick *video* query)))

(defn search-music ((query string)) reply
  (youtube 1 query "--no-video" "--ytdl-format=bestaudio/best")
  (reply "Music" query (pick *search* query)))

(defcommand media-search
  "!(play|put on|listen to) {text}"
  "!put [on] {text} on"
  "!(i|we) (want|would like|'d like|need) to (listen to|hear|play) {text}"
  "!(can|could|may) (i|we) (hear|listen to) {text}"
  "!(can|could|may) (i|we) get [some] (music|songs|tracks) by {text}"
  "!(let's|let me) (hear|listen to) {text}"
  :kind "Search"
  :slots ((query coalton:String))
  :parse (let ((q (search-query (slot "text")))) (and q (list q)))
  :examples ("Play Hey Jude" "Play some songs by Daft Punk")
  (search-music query))

;; "turn on" alone is a light or a setting.
(defcommand media-playlist
  "!turn on [the|my] {text} playlist"
  "!turn [the|my] {text} playlist on"
  "!(start|resume|pull up|use|do) [playing] [the|my] {text} playlist [please]"
  :kind "Search"
  :slots ((query coalton:String))
  :parse (let ((q (search-query (format nil "~a playlist" (slot "text"))))) (and q (list q)))
  :examples ("turn on the rap playlist")
  (search-music query))

(defn now-playing () reply
  "From `playerctl metadata`: artist|title|status."
  (let ((raw (output "playerctl" "metadata" "--format" "{{artist}}|{{title}}|{{status}}")))
    (if (null raw)
        (live "Now playing" "" (pick *nothing-playing*))
        (destructuring-bind (&optional (artist "") (title "") (status ""))
            (mapcar (lambda (s) (trim s)) (uiop:split-string raw :separator "|"))
          (let ((what (cond ((and (string= artist "") (string= title "")) nil)
                            ((and (string= artist "") (string= title *radio-tag*)) "the radio")
                            ((string= artist "") title)
                            (t (format nil "~a by ~a" title artist)))))
            (if (null what)
                (live "Now playing" ""
                      "Something is playing, but it won't tell me what. Mysterious.")
                (live "Now playing" what
                      (pick (if (string= status "Paused") *playing-paused* *playing*) what))))))))

(defcommand media-now-playing
  "!(what's|what is) (playing|this song|this track|the song|the track|on)"
  "!what (song|track|music) is (this|playing|that)"
  "!who (is|sings|plays) (this|that) [song|track]"
  "!(name|what's) (this|that) (song|tune)"
  :examples ("What's playing?" "Who sings this?")
  (now-playing))

(defskill media)

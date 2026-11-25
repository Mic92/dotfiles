;;; The Android TV in the bedroom, over adb on the network.

(in-package #:va)

(in-skill tv)

(defreplies *tv-on*
  "TV's on. Try to blink occasionally."
  "Waking the TV. The bed has been expecting you."
  "TV on. So much for an early night."
  "The big screen is up. I'm not jealous."
  (:late "TV on, at this hour? Just one episode. Sure."
         "TV on. Sleep is overrated anyway."))

(defreplies *tv-off*
  "TV off. Look, a room! With furniture!"
  "TV's off. Your eyes say thank you."
  "Screen off. Now it's just a very expensive mirror."
  "TV off. I'm the only entertainment left."
  (:late "TV off. Now sleep, both of you."
         "TV off. Good night. No, really this time."))

(defreplies *tv-louder*
  "TV louder. The neighbours wanted to follow the plot anyway."
  "Turning the TV up. Subtitles are for quitters."
  "Louder. Cinema mode.")

(defreplies *tv-quieter*
  "TV down. I can hear myself think again."
  "Quieter. The explosions will have to whisper."
  "Turning the TV down. Much more civilised.")

(defreplies *tv-volume*
  "TV volume ~a. As you wish."
  "TV at ~a. Happy now?"
  "~a on the TV. Precise. I like that in a human.")

(defreplies *tv-mute*
  "TV muted. Now you can make up the dialogue."
  "TV muted. It's a silent film now."
  "Muted. Lip-reading practice.")

(defreplies *tv-unmute*
  "TV sound's back. You missed nothing. Probably."
  "Unmuted. And they're still talking."
  "Sound on. Welcome back to the plot.")

(defreplies *tv-pause*
  "Paused. Crumbs in the bed again?"
  "TV paused. Nobody move."
  "Paused on a very unflattering frame.")

(defreplies *tv-play*
  "And, action!"
  "Rolling. Where were we?"
  "Back to the show. Shh.")

(defreplies *tv-next*
  "Next. Just one more episode, right?"
  "Skipping ahead. Who needs sleep, in a bedroom of all places."
  "Next one. I won't tell anyone how many that makes.")

(defreplies *tv-previous*
  "Going back. Fell asleep, did we?"
  "Back one. Pay attention this time."
  "Rewinding. It's still the same ending.")

(defreplies *tv-app*
  "~a, on the big screen."
  "~a on the TV. There goes the evening."
  "Putting on ~a. Find the remote yourself."
  "~a. I'll be here when you're done. Days from now.")

(defreplies *tv-unreachable*
  "The TV isn't answering. Playing dead, or actually unplugged."
  "I can't reach the TV. It's ignoring me. Rude."
  "No answer from the TV. Is it on the wifi?")

(defvar *tv-file* nil
  "The file with the TV's host, or host:port, on its first line, or nil for
voice-assistant/tv in the config directory.")

(defn tv-path () pathname (or *tv-file* (config-path "tv")))

(defn tv-serial () (or null string)
  "How adb names the TV: host:port."
  (let ((host (config-line (tv-path))))
    (and host (if (find #\: host) host (format nil "~a:5555" host)))))

(defn tv-shell (&rest (args string)) (or null string)
  "ARGS in the TV's shell: what they print, nil when the TV can't be reached."
  (let ((serial (tv-serial)))
    (and serial (apply #'run "timeout" "10" "adb" "-s" serial "shell" args))))

(defn tv-keys (&rest (keys string)) (or null string)
  (apply #'tv-shell "input" "keyevent" keys))

(defn on-tv ((speech string) (act function)) reply
  "SPEECH once ACT, which talks to the TV, returned true."
  (let ((serial (tv-serial)))
    (cond ((null serial)
           (reply "TV" (format nil "Put the TV's address on the first line of ~a" (tv-path))
                  "I don't know where your TV lives. Give me its address first."))
          ;; adb forgets the TV whenever either side restarts.
          ((progn (run "timeout" "3" "adb" "connect" serial) (funcall act))
           (reply "TV" "" speech))
          (t (live "TV" "" (pick *tv-unreachable*))))))

(defn after-tv-p () boolean
  "Whether the last command was for the TV."
  (let ((previous (context-previous *context*)))
    (and previous (eq (command-skill (action-command previous)) 'tv))))

(defmacro deftv (name keys lines templates follow-ups &rest rest)
  "A command pressing KEYS on the TV's remote. FOLLOW-UPS are templates not
naming the TV, which only count right after another TV command: \"louder\"."
  `(defcommand ,name ,@templates ,@follow-ups ,@rest
               :parse (or (not (member template ',follow-ups :test #'string=)) (after-tv-p))
               (on-tv (pick ,lines) (lambda () (tv-keys ,@keys)))))

(deftv tv-on ("KEYCODE_WAKEUP") *tv-on*
       ("(turn|switch|power|put) on [the|my] (tv|television|telly)"
        "(turn|switch|power|put) [the|my] (tv|television|telly) on"
        "(wake|wake up|start|fire up) [the|my] (tv|television|telly)"
        "!(tv|television|telly) on"
        "![i want to|i'd like to|let's|can i|can we] watch [some] (tv|television|telly)")
       ()
       :examples ("Turn on the TV." "Watch TV"))

(deftv tv-off ("KEYCODE_SLEEP") *tv-off*
       ("(turn|switch|power|shut) off [the|my] (tv|television|telly)"
        "(turn|switch|power|shut) [the|my] (tv|television|telly) off"
        "(power|shut) down [the|my] (tv|television|telly)"
        "(power|shut) [the|my] (tv|television|telly) down"
        "put [the|my] (tv|television|telly) to sleep"
        "kill [the|my] (tv|television|telly)"
        "!(tv|television|telly) off")
       ("!(turn|switch) it off")
       :examples ("Turn off the TV."))

;; The TV's volume goes from 0 to 100, a key press is one step.
(deftv tv-volume-up
    ("KEYCODE_VOLUME_UP" "KEYCODE_VOLUME_UP" "KEYCODE_VOLUME_UP" "KEYCODE_VOLUME_UP"
                         "KEYCODE_VOLUME_UP")
  *tv-louder*
  ("(turn|crank|pump) up [the] (volume|sound) (on|of) [the|my] (tv|television|telly)"
   "(turn|crank|pump) [the] (volume|sound) up (on|of) [the|my] (tv|television|telly)"
   "(increase|raise) [the] (volume|sound) (on|of) [the|my] (tv|television|telly)"
   "(turn|crank|pump|increase|raise) up [the|my] (tv|television|telly) [volume|sound]"
   "(increase|raise) [the|my] (tv|television|telly) (volume|sound)"
   "(turn|crank|pump) [the|my] (tv|television|telly) [volume|sound] up"
   "make [the|my] (tv|television|telly) louder"
   "(tv|television|telly) [volume|sound] (up|louder)"
   "louder on [the|my] (tv|television|telly)"
   "[the|my] (tv|television|telly) is [way|much|a bit] too (quiet|soft|low)"
   "i (can't|cannot|can barely) hear [the|my] (tv|television|telly)")
  ("!louder" "!(turn|crank) it up" "!(volume|sound) up")
  :repeatable t
  :examples ("Turn up the volume on the TV." "Make the TV louder"))

(deftv tv-volume-down
    ("KEYCODE_VOLUME_DOWN" "KEYCODE_VOLUME_DOWN" "KEYCODE_VOLUME_DOWN" "KEYCODE_VOLUME_DOWN"
                           "KEYCODE_VOLUME_DOWN")
  *tv-quieter*
  ("turn down [the] (volume|sound) (on|of) [the|my] (tv|television|telly)"
   "turn [the] (volume|sound) down (on|of) [the|my] (tv|television|telly)"
   "(lower|decrease|reduce) [the] (volume|sound) (on|of) [the|my] (tv|television|telly)"
   "turn down [the|my] (tv|television|telly) [volume|sound]"
   "(lower|decrease|reduce) [the|my] (tv|television|telly) (volume|sound)"
   "turn [the|my] (tv|television|telly) [volume|sound] down"
   "make [the|my] (tv|television|telly) (quieter|softer)"
   "(tv|television|telly) [volume|sound] (down|quieter|softer)"
   "(quieter|softer) on [the|my] (tv|television|telly)"
   "[the|my] (tv|television|telly) is [way|much|a bit] too loud")
  ("!(quieter|softer)" "!turn it down" "!(volume|sound) down")
  :repeatable t
  :examples ("Turn the TV down." "The TV is too loud"))

(defn tv-volume () (or null (integer 0))
  (let* ((out (tv-shell "cmd" "media_session" "volume" "--stream" "3" "--get"))
         (at (and out (search "volume is " out))))
    (and at (parse-integer out :start (+ at 10) :junk-allowed t))))

(defn tv-step-volume ((want (integer 0 100))) boolean
  "`media_session volume --set` is ignored by the TV, so walk there."
  (let* ((now (tv-volume))
         (steps (and now (- want now))))
    (and steps
         (or (zerop steps)
             (apply #'tv-keys
                    (make-list (abs steps) :initial-element (if (plusp steps)
                                                                "KEYCODE_VOLUME_UP"
                                                                "KEYCODE_VOLUME_DOWN"))))
         t)))

(defcommand tv-volume-set
  "[set|change|turn] [the|my] (tv|television|telly) [volume|sound] (to|at) {percent} [percent]"
  (:join "[set|change|turn] [the] (volume|sound) (on|of) [the|my] (tv|television|telly)"
         "(to|at) {percent} [percent]")
  "(tv|television|telly) (volume|sound) {percent} [percent]"
  "(volume|sound) [to|at] {percent} [percent] on [the|my] (tv|television|telly)"
  :slots ((percent coalton:UFix))
  :examples ("Set the TV volume to 30 percent.")
  (let ((want (min percent 100)))
    (on-tv (pick *tv-volume* want) (lambda () (tv-step-volume want)))))

(defn tv-set-mute ((mute boolean)) boolean
  "The remote's mute key only toggles, so look first."
  (let ((state (tv-shell "dumpsys audio | grep -A1 -e '^- STREAM_MUSIC:'")))
    (and state
         (or (eq mute (and (search "Muted: true" state) t))
             (tv-keys "KEYCODE_VOLUME_MUTE"))
         t)))

(defcommand tv-mute
  "(mute|silence) [the|my] (tv|television|telly)" "!(tv|television|telly) mute" "!mute [it]"
  :parse (or (not (string= template "!mute [it]")) (after-tv-p))
  :examples ("Mute the TV.")
  (on-tv (pick *tv-mute*) (lambda () (tv-set-mute t))))

;; "un mute", "on mute": how whisper tends to hear it.
(defcommand tv-unmute
  "unmute [the|my] (tv|television|telly)" "(un|on) mute [the|my] (tv|television|telly)"
  "!(tv|television|telly) unmute" "!unmute [it]"
  :parse (or (not (string= template "!unmute [it]")) (after-tv-p))
  :examples ("Unmute the TV.")
  (on-tv (pick *tv-unmute*) (lambda () (tv-set-mute nil))))

;; Whisper hears "pause TV" as "post TV". A bare "stop" stays the timer's.
(deftv tv-pause ("KEYCODE_MEDIA_PAUSE") *tv-pause*
       ("(pause|post|stop|freeze|hold) [the|my] (tv|television|telly)"
        (:join "(pause|post|stop|freeze) [the|this|my|it]"
               "[movie|film|show|video|episode|series|playback] on [the|my] (tv|television|telly)")
        "!(pause|post|stop|freeze) {streaming}")
       ("!(pause|post) [it|that]")
       :examples ("Pause the TV."))

(deftv tv-play ("KEYCODE_MEDIA_PLAY") *tv-play*
       ("(resume|continue|unpause|play) [the|my] (tv|television|telly)"
        (:join "(resume|continue|unpause|play) [the|this|my|it]"
               "[movie|film|show|video|episode|series|playback] on [the|my] (tv|television|telly)")
        "!(resume|continue|unpause) {streaming}"
        "!(continue|resume|keep) watching")
       ("!(resume|continue|unpause|play) [it|that]")
       :examples ("Resume the TV." "Continue watching"))

(deftv tv-next ("KEYCODE_MEDIA_NEXT") *tv-next*
       ("[play] [the] next [episode|video|one|song|track] on [the|my] (tv|television|telly)"
        "skip [this|the] [episode|video|one|song|track] on [the|my] (tv|television|telly)"
        "!(tv|television|telly) (next|skip)")
       ("![play] [the] next [episode|video|one]" "!skip [it|this|this one|that]")
       :repeatable t
       :examples ("Next episode on the TV."))

(deftv tv-previous ("KEYCODE_MEDIA_PREVIOUS") *tv-previous*
       ((:join "[play] [the] (previous|last) [episode|video|one|song|track]"
               "on [the|my] (tv|television|telly)")
        "go back [a|one|an] [episode|video|song|track] on [the|my] (tv|television|telly)"
        "!(tv|television|telly) (previous|back)")
       ("![play] [the] previous [episode|video|one]" "!go back")
       :repeatable t
       :examples ("Previous episode on the TV."))

#+mallet
(declaim (mallet:suppress-next :line-length))
(defparameter *tv-apps*
  '((("youtube" "you tube") "YouTube"
     "com.google.android.youtube.tv/com.google.android.apps.youtube.tv.activity.ShellActivity")
    (("youtube music" "you tube music") "YouTube Music"
     "com.google.android.youtube.tvmusic/com.google.android.apps.youtube.tvmusic.activity.MainActivity")
    (("netflix") "Netflix" "com.netflix.ninja/.MainActivity")
    (("prime video" "amazon prime" "amazon prime video" "amazon video" "amazon" "prime")
     "Prime Video" "com.amazon.amazonvideo.livingroom/com.amazon.ignition.IgnitionActivity")
    (("disney plus" "disney") "Disney+"
     "com.disney.disneyplus/com.bamtechmedia.dominguez.main.MainActivity")
    (("spotify") "Spotify" "com.spotify.tv.android/com.spotify.app.androidtv.MainActivity")
    (("jellyfin" "jelly fin") "Jellyfin" "org.jellyfin.androidtv/.ui.startup.StartupActivity")
    (("ard" "ard mediathek" "das erste") "ARD Mediathek" "de.swr.avp.ard.tv/.TvActivity")
    (("zdf" "zdf mediathek") "ZDF Mediathek" "com.zdf.android.mediathek/.tv.MainActivity")
    (("arte") "Arte" "tv.arte.plus7/.leanback.MainActivity")
    (("joyn" "join") "Joyn" "de.prosiebensat1digital.seventv/.MainActivity")
    (("dropout") "Dropout" "com.collegehumor.chdropout/.MainActivity")
    (("beacon") "Beacon" "tv.beacon.ott/tv.accedo.one.app.bootstrap.BootstrapActivity")
    (("newpipe" "new pipe") "NewPipe"
     "InfinityLoop1309.NewPipeEnhanced/org.schabi.newpipe.MainActivity")
    (("moonlight") "Moonlight" "com.limelight/.PcView")
    (("play store" "app store") "the Play Store"
     "com.android.vending/com.google.android.finsky.tvmainactivity.TvMainActivity" t)
    (("settings") "the TV settings" "com.android.tv.settings/.MainSettings" t)
    (("browser" "web browser") "the TV browser"
     "com.tcl.browser/.portal.home.activity.HomePageActivity" t)
    (("files" "file manager") "the TV's files"
     "me.zhanghai.android.files/.filelist.FileListActivity" t))
  "Spoken names, the label shown and spoken, the activity to start, and
whether the desktop has an app of that name too.")

(defn tv-app-at ((i (integer 0))) list (nth i *tv-apps*))

(defn tv-app-lookup ((ws words) (shared boolean)) (or null (integer 0))
  "The TV app named by exactly WS. SHARED takes the names the desktop has an
app for too."
  (let ((spoken (unwords ws)))
    (position-if (lambda (app)
                   (and (or shared (not (fourth app)))
                        (member spoken (first app) :test #'string=)))
                 *tv-apps*)))

(defn tv-app-names ((shared boolean)) list
  (loop for (names nil nil desktop) in *tv-apps*
        when (or shared (not desktop)) append (mapcar #'split-words names)))

(defslot "tvapp" (ws next) (tv-app-lookup ws t))
(closed-slot "tvapp" (lambda () (tv-app-names t)))

;; Apps that can be named without "on the tv": "open the browser" is the desktop's.
(defslot "streaming" (ws next) (tv-app-lookup ws nil))
(closed-slot "streaming" (lambda () (tv-app-names nil)))

(defcommand tv-app
  (:join "(open|launch|start|show|put on|put|play|switch to|watch) [the|my] {tvapp} [app]"
         "on [the|my] (tv|television|telly)")
  "(switch|turn|set) [the|my] (tv|television|telly) to {tvapp}"
  "!{tvapp} on [the|my] (tv|television|telly)"
  "!(open|launch|start|put on|switch to) [the|my] {streaming} [app]"
  "![i want to|i'd like to|let's|can i|can we] watch [some] {streaming}"
  :slots ((app coalton:UFix))
  :parse (list (or (slot "tvapp") (slot "streaming")))
  :examples ("Open Netflix on the TV." "Watch Netflix")
  (destructuring-bind (&optional names label activity &rest desktop) (tv-app-at app)
    (declare (ignore names desktop))
    (if (null activity)
        (reply "TV" "" "I don't know that app. Awkward.")
        (on-tv (pick *tv-app* label)
               (lambda () (tv-shell "am" "start" "-n" activity))))))

(defskill tv)

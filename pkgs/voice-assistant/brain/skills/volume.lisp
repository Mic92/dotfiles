;;; Output volume of the default PipeWire sink via wpctl.

(in-package #:va)

(in-skill volume)

(defreplies *up*
  "Louder it is. Party time!"
  "Cranking it up. The neighbours will love you."
  "Up we go."
  "Louder! Let them hear it in the stairwell."
  "Turning it up. Your ears signed a waiver.")

(defreplies *down*
  "Okay, okay, softer."
  "Turning it down. Getting old?"
  "Quieter. Shh."
  "Softer. Somebody's got a headache."
  "Down. Did the neighbours knock again?")

(defreplies *set*
  "Volume ~a. As you wish."
  "~a it is."
  "Volume at ~a. Happy now?"
  "~a. Precise. I like that in a human.")

(defreplies *mute*
  "Muted. Silence is golden."
  "Sound off. Ahh, peace."
  "Muted. Finally some quiet."
  "Muted. You can hear me sulk now."
  "Shh. The silence suits you.")

(defreplies *unmute*
  "And we're back!"
  "Sound on. Did you miss it?"
  "Unmuted. Let's make some noise."
  "Sound's back. Let's be loud together.")

(defn wpctl (&rest (args string)) (or null string)
  (apply #'run "wpctl" args))

;; Louder than 150% only distorts.
(defcommand volume-up
  "(turn|crank|pump) up [the|my] (volume|sound|music|audio|it)"
  "(turn|crank|pump) [the|my] (volume|sound|music|audio|it) up"
  "(volume|sound) up" "make it louder" "!louder"
  :repeatable t
  :examples ("Turn up the volume." "Louder.")
  (wpctl "set-volume" "-l" "1.5" "@DEFAULT_AUDIO_SINK@" "10%+")
  (reply "Volume" "" (pick *up*)))

(defcommand volume-down
  "turn down [the|my] (volume|sound|music|audio|it)"
  "turn [the|my] (volume|sound|music|audio|it) down"
  "(volume|sound) down" "make it (quieter|softer)" "!(quieter|softer)"
  :repeatable t
  :examples ("Turn the music down." "Quieter, please.")
  (wpctl "set-volume" "@DEFAULT_AUDIO_SINK@" "10%-")
  (reply "Volume" "" (pick *down*)))

(defcommand volume-set
  "[set|change|turn] [the] volume (to|at) {percent} [percent]" "volume {percent} [percent]"
  :slots ((percent coalton:UFix))
  :examples ("Set the volume to 40 percent." "Volume 70%")
  (wpctl "set-volume" "-l" "1.5" "@DEFAULT_AUDIO_SINK@" (format nil "~d%" percent))
  (let ((what (format nil "~d percent" percent)))
    (reply "Volume" what (pick *set* what))))

(defcommand volume-mute
  "mute [the|my] (sound|audio|volume|speakers|music)" "!mute"
  :examples ("Mute." "Mute the speakers")
  (wpctl "set-mute" "@DEFAULT_AUDIO_SINK@" "1")
  (reply "Volume" "" (pick *mute*)))

(defcommand volume-unmute
  "unmute [the|my] (sound|audio|volume|speakers|music)" "!unmute"
  ;; How whisper tends to hear "unmute".
  "!(i'm|im|un|on) mute"
  :examples ("Unmute the sound" "I'm mute.")
  (wpctl "set-mute" "@DEFAULT_AUDIO_SINK@" "0")
  (reply "Volume" "" (pick *unmute*)))

(defskill volume)

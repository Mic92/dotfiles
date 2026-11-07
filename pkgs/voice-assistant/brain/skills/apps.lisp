;;; Open applications from a fixed list.

(in-package #:va)

(in-skill app)

(defreplies *opening*
  "Opening ~a."
  "~a, coming right up."
  "Here's ~a. Don't say I never do anything for you."
  "~a. Anything else, your majesty?"
  "~a. Try not to get distracted this time."
  "Launching ~a. I'd say break a leg, but you might."
  "~a. Opening it before you change your mind.")

(defparameter *apps*
  '((("browser" "web browser" "librewolf" "firefox" "internet" "web")
     "the browser" ("librewolf"))
    (("terminal" "shell" "console" "ghostty")
     "a terminal" ("ghostty"))
    (("signal" "messenger" "chat")
     "Signal" ("signal-desktop"))
    (("mail" "email" "e mail" "inbox" "aerc")
     "your mail" ("ghostty" "-e" "aerc"))
    (("files" "file manager" "yazi")
     "the file manager" ("ghostty" "-e" "yazi"))
    (("editor" "text editor" "vim" "neovim" "nvim")
     "the editor" ("ghostty" "-e" "nvim"))
    (("system monitor" "task manager" "htop" "processes")
     "the system monitor" ("ghostty" "-e" "htop"))
    (("sound settings" "audio settings" "mixer" "volume control" "pavucontrol")
     "the sound settings" ("pavucontrol"))
    (("gimp" "image editor" "photo editor")
     "GIMP" ("gimp"))
    (("pdf viewer" "document viewer" "evince")
     "the document viewer" ("evince"))
    (("settings" "preferences" "control center")
     "the settings" ("noctalia" "msg" "settings-open")))
  "Spoken names, the label shown and spoken, and the command.")

(defn app-lookup ((ws words)) (or null (integer 0))
  "The app named by exactly WS."
  (let ((spoken (unwords ws)))
    (position-if (lambda (app) (member spoken (first app) :test #'string=)) *apps*)))

(defslot "app" (ws next) (app-lookup ws))

(defn capitalize-first ((s string)) string
  (if (plusp (length s)) (concatenate 'string (string-upcase (subseq s 0 1)) (subseq s 1)) s))

(defcommand app-launch
  "(open|launch|start|run|show) [the|my|a|an] {app} [app|application|window]"
  "(open|launch|start) [up] [the|my|a|an] {app}"
  "i (need|want) [the|my|a|an] {app}"
  :slots ((app coalton:UFix))
  :examples ("Open the browser." "Launch a terminal")
  (destructuring-bind (&optional names label command) (nth app *apps*)
    (declare (ignore names))
    (cond ((null label) (reply "Unknown app" "" "I don't know that app. Awkward."))
          (t (apply #'spawn command)
             (reply "Opening" label (capitalize-first (pick *opening* label)))))))

(defskill app)

;;; Desktop toggles and actions via noctalia and niri.

(in-package #:va)

(in-skill desktop)

(defreplies *done*
  "Done."
  "As you wish."
  "Your wish is my command."
  "Consider it done, gorgeous."
  "Done. You owe me one."
  "Sorted. What would you do without me?"
  "There. Was that so hard? For me, no.")

(defreplies *brighter*
  "Let there be light!"
  "Brighter. Like your future."
  "Ow, my eyes. Done."
  "Brighter. Now I can see your bad posture."
  "Lights up. Hide the evidence.")

(defreplies *darker*
  "Dimming. Very romantic."
  "Darker. Setting the mood?"
  "Lights down, vibe up."
  "Dimmer. Hiding something?"
  "Darker. Candles not included.")

(defreplies *night*
  "Night light on. Cozy."
  "Warm colours activated. Bedtime soon, okay?"
  "Warm light. Your eyes say thank you, your bedtime says go.")

(defreplies *dnd*
  "Do not disturb. Not even me? Okay, not even me."
  "Shields up. Nobody gets through."
  "Do not disturb. Except me. I'm the exception."
  "Ghost mode. Everyone else can wait.")

(defreplies *screenshot*
  "Cheese!"
  "Snap! Hope there's nothing embarrassing on there."
  "Got it. Evidence secured."
  "Screenshot taken. Blackmail folder updated."
  "Click. Say cheese next time.")

(defreplies *wallpaper*
  "New wallpaper. Ooh, fancy."
  "Redecorating. Do you like it?"
  "Fresh wallpaper. Fresh start. Same old you.")

(defreplies *caffeine*
  "Caffeine on. I'm wide awake now. Too awake."
  "Screen stays on. No naps for you."
  "The screen will stay on. Unlike your attention span.")

(defreplies *close*
  "Closed. Bye bye window."
  "Gone. I never liked that one anyway."
  "Closed. It had it coming."
  "Window gone. Your secrets with it.")

(defreplies *off*
  "Turned off."
  "Off it goes."
  "Done. Off.")

(defn noctalia (&rest (args string)) null
  "noctalia's IPC, which shows its OSD for brightness and friends."
  (apply #'spawn "noctalia" "msg" args))

(defn desk ((lines replies) &rest (args string)) reply
  (apply #'noctalia args)
  (reply "Desktop" "" (pick lines)))

(defn toggle ((name string) (on boolean) (lines replies)) reply
  "`<name>-enable` or `<name>-disable`. Turning off gets the plain replies."
  (desk (if on lines *off*) (format nil "~a-~:[disable~;enable~]" name on)))

(defmacro deftoggle (name kind on templates examples &body body)
  "A command for one side of an on/off pair, the field ON being ON."
  `(defcommand ,name ,@templates
               :kind ,kind
               :slots ((on coalton:Boolean))
               :parse (list ,on)
               :examples ,examples
               ,@body))

(defcommand desktop-brighter
  "(make|turn) [the] (screen|display|brightness) (brighter|up)" "(turn|crank) up the brightness"
  "brightness up" "!brighter"
  :repeatable t
  :examples ("Make the screen brighter." "Brighter!")
  (desk *brighter* "brightness-up"))

(defcommand desktop-darker
  "(make|turn) [the] (screen|display|brightness) (darker|dimmer|down)" "turn down the brightness"
  "dim the (screen|display)" "brightness down" "!(darker|dimmer)"
  :repeatable t
  :examples ("Dim the screen")
  (desk *darker* "brightness-down"))

(defcommand desktop-brightness
  "[set|change] [the] brightness (to|at) {number} [percent]" "brightness {number} [percent]"
  :slots ((percent coalton:UFix))
  :parse (let ((n (slot "number"))) (and (<= 0 n 100) (list (round n))))
  :examples ("Set the brightness to 40 percent")
  (desk *done* "brightness-set" (princ-to-string percent)))

(deftoggle desktop-night-light-on "NightLight" t
    ("(turn|switch) on [the] night (light|mode)" "(enable|start) [the] night (light|mode)"
                                                 "night (light|mode) on" "!night (light|mode)")
    ("Turn on the night light.")
  (toggle "nightlight" on *night*))

(deftoggle desktop-night-light-off "NightLight" nil
    ("(turn|switch) off [the] night (light|mode)" "(disable|stop) [the] night (light|mode)"
                                                  "night (light|mode) off")
    ("Night mode off")
  (toggle "nightlight" on *night*))

(deftoggle desktop-wifi-on "Wifi" t
    ("(turn|switch) on [the] (wifi|wi|wireless) [fi]" "enable [the] (wifi|wi|wireless) [fi]"
                                                      "(wifi|wi) [fi] on")
    ("Turn on the Wi-Fi")
  (toggle "wifi" on *done*))

(deftoggle desktop-wifi-off "Wifi" nil
    ("(turn|switch) off [the] (wifi|wi|wireless) [fi]"
     "disable [the] (wifi|wi|wireless) [fi]"
     "(wifi|wi) [fi] off")
    ("Turn off the wifi.")
  (toggle "wifi" on *done*))

(deftoggle desktop-bluetooth-on "Bluetooth" t
    ("(turn|switch) on [the] bluetooth" "enable [the] bluetooth" "bluetooth on")
    ("Turn on bluetooth")
  (toggle "bluetooth" on *done*))

(deftoggle desktop-bluetooth-off "Bluetooth" nil
    ("(turn|switch) off [the] bluetooth" "disable [the] bluetooth" "bluetooth off")
    ("Turn off bluetooth")
  (toggle "bluetooth" on *done*))

(defn do-not-disturb ((on boolean)) reply
  (desk (if on *dnd* *off*) "notification-dnd-set" (if on "on" "off")))

(deftoggle desktop-dnd-on "DoNotDisturb" t
    ("(turn|switch) on do not disturb" "(enable|activate) do not disturb"
                                       "!do not disturb [mode] [on]" "!dnd" "!focus mode")
    ("Do not disturb.")
  (do-not-disturb on))

(deftoggle desktop-dnd-off "DoNotDisturb" nil
    ("(turn|switch) off do not disturb" "(disable|deactivate) do not disturb"
                                        "do not disturb off")
    ("Turn off do not disturb")
  (do-not-disturb on))

(defcommand desktop-screenshot
  "(take|make|grab) a screenshot" "!screenshot" "screenshot [of] [the] [whole] screen"
  :examples ("Take a screenshot.")
  (desk *screenshot* "screenshot-fullscreen"))

(defcommand desktop-screenshot-region
  "screenshot [of] [a|the] (region|area|part|selection)"
  "(take|make) a (region|partial) screenshot"
  :examples ("Screenshot of a region")
  (desk *screenshot* "screenshot-region"))

(defcommand desktop-wallpaper
  "(change|switch|next|new|random) [the|my] wallpaper"
  "(give|show) me a (new|different) wallpaper"
  :repeatable t
  :examples ("Change the wallpaper")
  (desk *wallpaper* "wallpaper-random"))

(deftoggle desktop-caffeine-on "Caffeine" t
    ("keep the (screen|display|computer) (on|awake)" "(don't|do not) (sleep|lock|dim)"
                                                     "(turn|switch) on caffeine" "!caffeine")
    ("Keep the screen on.")
  (toggle "caffeine" on *caffeine*))

(deftoggle desktop-caffeine-off "Caffeine" nil
    ("(turn|switch) off caffeine" "let the (screen|display|computer) sleep")
    ("Let the screen sleep")
  (toggle "caffeine" on *caffeine*))

(defmacro defprofile (name profile &rest templates)
  `(defcommand ,name ,@templates
               :kind "PowerProfile"
               :slots ((profile coalton:String))
               :parse (list ,profile)
               :examples ,(list (first templates))
               (desk *done* "power-set" profile)))

(defprofile desktop-power-saver "power-saver"
  "(power|battery|energy) (saving|saver) [mode]" "save (power|battery|energy)")
(defprofile desktop-performance "performance"
  "performance mode" "(full|max|maximum) (power|performance)")
(defprofile desktop-balanced "balanced" "balanced [power] mode")

(defcommand desktop-launcher
  "(open|show) [the] [app|application] launcher" "show [me] [the|my] apps"
  :examples ("Open the launcher")
  (desk *done* "panel-toggle" "launcher"))

(defcommand desktop-clear-notifications
  "(clear|dismiss|delete) [all] [the|my] notifications"
  :examples ("Clear all notifications.")
  (desk *done* "notification-clear-history"))

(defcommand desktop-close-window
  "close [this|the] (window|app|application)"
  :examples ("Close this window")
  (spawn "niri" "msg" "action" "close-window")
  (reply "Desktop" "" (pick *close*)))

(defcommand desktop-fullscreen
  "(make|go|toggle) [it|this] fullscreen" "make the window fullscreen" "!fullscreen"
  "maximize [this|the] window"
  :examples ("Fullscreen.")
  (spawn "niri" "msg" "action" "fullscreen-window")
  (reply "Desktop" "" (pick *done*)))

(defskill desktop)

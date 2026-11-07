;;; Machines, network, background jobs and resources.

(in-package #:va)

(in-skill ops)

(defparameter *ip-url* "https://ip.thalheim.io"
  "Answers with the address the request came from.")

(defparameter *hosts*
  '(("eve.r" (("eve") ("the" "server")))
    ("eva.r" (("eva") ("ava")))
    ("blob64.r" (("blob" "64") ("blob")))
    ("evo.r" (("evo")))
    ("turingmachine.r" (("turing" "machine") ("turingmachine") ("the" "desktop")))
    ("bernie.r" (("bernie") ("the" "laptop"))))
  "Hosts to ping and their spoken names, the first one said back. Commands carry
indices.")

(defparameter *bt-devices*
  '(("Audeze" "headphones"
     (("headphones") ("headphone") ("headset") ("audeze") ("maxwell") ("big" "headphones")))
    ("Beats" "earbuds" (("earbuds") ("earphones") ("beats") ("in" "ears") ("beats" "flex")))
    ("MX Master" "mouse" (("mouse") ("mx" "master"))))
  "Paired devices: a substring of their name, what we call them, spoken names.")

(defn named-row ((table list) (ws words)) (or null (integer 0))
  "Which row of TABLE lists WS among the names in its last element."
  (position-if (lambda (row) (member ws (a:lastcar row) :test #'equal)) table))

(defslot "host" (ws next) (named-row *hosts* ws))
(defslot "bt" (ws next) (named-row *bt-devices* ws))
(closed-slot "host" (lambda () (mapcan (lambda (r) (copy-list (a:lastcar r))) *hosts*)))
(closed-slot "bt" (lambda () (mapcan (lambda (r) (copy-list (a:lastcar r))) *bt-devices*)))

(defn succeeds ((program string) &rest (args string)) boolean
  (and (apply #'run program args) t))

(defn ping ((host string)) boolean (succeeds "ping" "-c" "1" "-W" "2" host))

(defreplies *ping-filler*
  "Checking." "One moment, pinging." "Poking the network with a stick."
  "Knock knock. Let's see who's home.")

(defreplies *host-up*
  "~a is up and purring." "~a answers. Alive and well." "Yep, ~a is fine. Stop worrying."
  "~a is up. Unlike you before your first Mate.")

(defreplies *host-down*
  "~a is not answering. Uh oh." "~a is down. Or ignoring us. Rude either way."
  "No reply from ~a. Time to panic? A little?" "~a is ghosting us. Rude.")

(defn host-up ((i (integer 0))) reply
  (destructuring-bind (host names) (nth i *hosts*)
    (let ((name (string-capitalize (unwords (first names)) :end 1)))
      (if (ping host)
          (live "Hosts" (format nil "~a is up" name) (pick *host-up* name))
          (live "Hosts" (format nil "~a is down" name) (pick *host-down* name))))))

(defreplies *online*
  "The internet works. Whatever is broken, it's you." "Online. Cat videos are a go."
  "Online. Go on, doomscroll.")

(defreplies *offline*
  "We're offline. Go outside, I guess."
  "No internet. Have you tried turning it off and on again?"
  "Offline. Read a book. Remember those?")

(defn internet () reply
  (cond ((not (ping "1.1.1.1")) (live "Network" "offline" (pick *offline*)))
        ((ping "eve.thalheim.io") (live "Network" "online" (pick *online*)))
        (t (live "Network" "online"
                 "I can reach the internet, but not names. Smells like DNS. It's always DNS."))))

(defn local-ip () (or null string)
  "From \"1.1.1.1 via 192.168.1.1 dev wlan0 src 192.168.1.23 uid 1000\"."
  (second (member "src" (split-words (or (run "ip" "-4" "route" "get" "1.1.1.1") ""))
                  :test #'string=)))

(defn ip-speech ((local (or null string)) (public (or null string)) (v6 (or null string)))
    string
  (cond (public (format nil "Your public IP is ~a. ~:[No IPv6. How retro.~;IPv6 too, it's in the ~
notification.~] Don't give it to strangers." public v6))
        (local (format nil "Your local address is ~a. The outside world is unreachable." local))
        (t "No IP at all. We're floating in the void.")))

(defn public-ip ((ip (member :v4 :v6))) (or null string)
  (let ((s (http-get *ip-url* :timeout 4 :ip ip))) (and s (trim s))))

(defn ip () reply
  (let ((local (local-ip)) (public (public-ip :v4)) (v6 (public-ip :v6)))
    (live "IP address"
          (format nil "~{~a~^~%~}" (remove nil (list (and local (format nil "local ~a" local))
                                                     (and public (format nil "public ~a" public))
                                                     (and v6 (format nil "IPv6 ~a" v6)))))
          (ip-speech local public v6))))

(defreplies *no-disk*
  "I can't read the disk. Spooky." "The disk won't talk to me. Probably full of your downloads.")
(defreplies *disk-full*
  "~a. We're full! Delete something. Maybe old nix generations?"
  "~a. Run garbage collection, I'm begging you.")
(defreplies *disk-tight* "~a. Getting tight. Nix store again?" "~a. Not critical, but I'm judging.")
(defreplies *disk-ok* "~a. Plenty of room." "~a. Hoard away.")

(defn disk () reply
  "From df's \"Avail Use%\" and \"123G  45%\"."
  (let ((row (split-words (or (second (uiop:split-string (or (run "df" "--output=avail,pcent" "-BG"
                                                                  "/home")
                                                             "")
                                                         :separator '(#\Newline)))
                              ""))))
    (if (/= (length row) 2)
        (live "Disk" "" (pick *no-disk*))
        (let* ((avail (or (parse-integer (first row) :junk-allowed t) 0))
               (pct (or (parse-integer (second row) :junk-allowed t) 0))
               (what (format nil "~d gigabytes free, ~d percent used" avail pct)))
          (live "Disk" what (pick (cond ((>= pct 95) *disk-full*) ((>= pct 80) *disk-tight*)
                                        (t *disk-ok*))
                                  what))))))

(defn meminfo ((raw string) (field string)) (or null (integer 0))
  "FIELD's kilobytes in /proc/meminfo text."
  (cl-ppcre:register-groups-bind ((#'parse-integer kb))
                                 ((format nil "(?m)^~a:\\s+(\\d+)" field) raw)
                                 kb))

(defreplies *no-memory* "I forgot how much memory we have. Ironic." "Memory? What memory? Exactly.")
(defreplies *memory-full* "~a. Close some browser tabs, hoarder." "~a. I can barely think in here.")
(defreplies *memory-ok* "~a." "~a. Room for more tabs. Unfortunately.")

(defn memory () reply
  (let* ((raw (read-proc "/proc/meminfo"))
         (total (meminfo raw "MemTotal")) (avail (meminfo raw "MemAvailable")))
    (if (not (and total avail))
        (live "Memory" "" (pick *no-memory*))
        (let* ((used (- 100 (floor (* avail 100) (max total 1))))
               (what (format nil "~d gigabytes free, ~d percent used" (floor avail (* 1024 1024))
                             used)))
          (live "Memory" what (pick (if (>= used 90) *memory-full* *memory-ok*) what))))))

(defreplies *cpu-calm*
  "Chill. ~a. I'm barely awake." "~a. The CPU is napping. Lucky thing."
  "~a. Nothing to worry about.")

(defn top-process () (values (or null string) real)
  "The busiest process and its CPU percentage, from \"firefox  123.4\"."
  (let ((line (first (split-lines (or (run "ps" "-eo" "comm,%cpu" "--sort=-%cpu" "--no-headers")
                                      "")))))
    (cl-ppcre:register-groups-bind (name pct) ("^\\s*(.*\\S)\\s+([\\d.]+)\\s*$" (or line ""))
                                   (return-from top-process (values name (or (parse-real pct) 0))))
    (values nil 0)))

(defn cpu () reply
  (let* ((load (or (parse-real (or (first (split-words (read-proc "/proc/loadavg"))) "")) 0d0))
         (cores (or (parse-integer (or (run "nproc") "") :junk-allowed t) 1))
         (what (format nil "load ~,1f on ~d cores" load cores)))
    (multiple-value-bind (name pct) (top-process)
      (live "CPU" what
            (cond ((and name (>= pct 50))
                   (format nil "~a is hogging the CPU at ~d percent. Want me to judge it?" name
                           (round pct)))
                  ((>= (/ (* load 100) cores) 80)
                   (format nil "Busy! ~a. Something is compiling, I bet." what))
                  (t (pick *cpu-calm* what)))))))

(defreplies *no-phone*
  "Your phone isn't connected. Check the couch cushions."
  "I can't reach your phone. It's hiding from you. Smart phone."
  "No phone in range. Did you leave it at the bar again?"
  "No phone. Check the fridge, that's where it was last time.")
(defreplies *phone-rings*
  "Ringing it now. Follow the noise." "Your phone is screaming somewhere. Go find it."
  "Ring ring! Marco... Polo." "It's ringing. Follow the sound of your shame.")

(defn find-phone () reply
  (let ((ids (split-words (or (run "kdeconnect-cli" "--list-available" "--id-only") ""))))
    (cond ((null ids) (live "Find phone" "no device connected" (pick *no-phone*)))
          (t (dolist (id ids) (spawn "kdeconnect-cli" "--device" id "--ring"))
             (live "Find phone" "ringing" (pick *phone-rings*))))))

(defn bt-mac ((devices string) (needle string)) (or null string)
  "The MAC of the first \"Device E0:49:ED:04:B7:B4 Audeze Maxwell BT\" naming NEEDLE."
  (loop for line in (split-lines devices)
        for (mac name) = (cl-ppcre:register-groups-bind (mac name) ("^\\s*Device (\\S+) (.*)" line)
                                                        (list mac name))
        when (and name (search needle name :test #'char-equal)) return mac))

(defreplies *bt-connect*
  "Connecting your ~a. Give it a second." "~a, coming right up."
  "Calling your ~a. Hope they pick up." "Pairing up with your ~a. Get a room, you two.")
(defreplies *bt-disconnect*
  "Disconnecting your ~a." "Bye bye, ~a." "~a dumped. It's not you, it's Bluetooth."
  "~a cut loose.")

(defparameter *bt-connect-script*
  "bluetoothctl power on >/dev/null; exec bluetoothctl connect \"$0\""
  "Connects the MAC in $0, powering the adapter on first.")

(defn bluetooth ((device (integer 0)) (connect boolean)) reply
  (destructuring-bind (needle what names) (nth device *bt-devices*)
    (declare (ignore names))
    (let ((mac (bt-mac (or (run "bluetoothctl" "devices" "Paired") "") needle)))
      (cond ((null mac)
             (live "Bluetooth" ""
                   (format nil "I can't find your ~a among the paired devices. Pair first, I'm not ~
a magician." what)))
            (t
             ;; Connecting blocks for seconds, the reply should not wait for it.
             (spawn "sh" "-c" (if connect *bt-connect-script* "exec bluetoothctl disconnect \"$0\"")
                    mac)
             (live "Bluetooth" (format nil "~:[disconnect~;connect~] ~a" connect needle)
                   (pick (if connect *bt-connect* *bt-disconnect*) what)))))))

(defn refine-host ((previous action) (ws words)) (or null action)
  "\"and eva?\""
  (declare (ignore previous))
  (let ((i (named-row *hosts* (strip-one ws "the" "my")))) (and i (command-action 'ops-host-up i))))

(defn refine-bt ((previous action) (ws words)) (or null action)
  "\"and the mouse?\""
  (let ((i (named-row *bt-devices* (strip-one ws "the" "my"))))
    (and i (command-action (command-name (action-command previous)) i
                           (second (action-fields previous))))))

(defcommand ops-host-up
  "!(is|are) {host} (up|down|online|offline|alive|reachable|running)"
  "!can you (reach|ping) {host}"
  "!ping {host}"
  "!(check|how is|how's) {host}"
  :kind "HostUp"
  :slots ((host coalton:UFix))
  :refine #'refine-host
  :filler *ping-filler*
  :examples ("is eve up" "ping the laptop")
  (host-up host))

(defcommand ops-internet
  "(is|are) [the|my] (internet|network|wifi|connection) (down|up|working|broken|ok|slow)"
  "(am|are) (i|we) (online|offline|connected)"
  "do i have internet"
  "!ping [the] internet"
  :filler *ping-filler*
  :examples ("is the internet down" "do i have internet")
  (internet))

(defcommand ops-ip
  "what's my ip [address]"
  "what is my ip [address]"
  "!ip address"
  :filler *ping-filler*
  :examples ("what's my ip address")
  (ip))

(defcommand ops-disk
  "how much (disk|disc|storage) [space] [is] [left|free|do i have]"
  "(is|am) [the|my] (disk|disc) [running] (full|out of space)"
  "!(disk|disc) space"
  :examples ("how much disk space is left")
  (disk))

(defcommand ops-memory
  "how much (memory|ram) [is] [left|free|used|do i have]"
  "!(memory|ram) [usage]"
  :examples ("how much memory is free")
  (memory))

(defcommand ops-cpu
  "why is [the|my] (computer|laptop|fan) [so] (slow|loud|hot)"
  "what's (using|eating|hogging) [all] [the|my] (cpu|processor|memory)"
  "how busy is [the|my] (computer|cpu|machine)"
  "!(cpu|load) [usage]"
  :examples ("why is my laptop so slow")
  (cpu))

(defcommand ops-find-phone
  "!(find|ring|locate|where is|where's) my (phone|android|mobile|cell phone|handy)"
  "!(i|i've) (lost|can't find|cannot find) my (phone|mobile|handy)"
  "!(make|let) my phone (ring|beep)"
  :examples ("find my phone" "i lost my phone")
  (find-phone))

(defcommand ops-connect
  "!(connect|pair|hook up) [my|the] {bt} [please]"
  "!(connect|switch) [the] (audio|sound) to [my|the] {bt}"
  "!(use|switch to) [my|the] {bt}"
  :kind "Bluetooth"
  :named t
  :slots ((device coalton:UFix) (connect coalton:Boolean))
  :parse (list (slot "bt") t)
  :refine #'refine-bt
  :examples ("connect my headphones" "switch to the earbuds")
  (bluetooth device connect))

(defcommand ops-disconnect
  "!(disconnect|unpair|unhook) [my|the] {bt}"
  :kind "Bluetooth"
  :named t
  :slots ((device coalton:UFix) (connect coalton:Boolean))
  :parse (list (slot "bt") nil)
  :refine #'refine-bt
  :examples ("disconnect my headphones")
  (bluetooth device connect))

(defskill ops)

;;; The Android TV, driven over adb.

(in-package #:va-tests)

(defmacro with-tv ((&optional (line "tv.example")) &body body)
  "BODY with LINE as the TV's config file."
  `(uiop:with-temporary-file (:pathname tv :stream out)
     (write-line ,line out)
     (finish-output out)
     (let ((va::*tv-file* tv)) ,@body)))

(defun tv-ran (text)
  "What saying TEXT sends to the TV's shell."
  (loop for (nil nil . args) in (va:fake-effects (va::perform (va::parse-text text)))
        when (eql 4 (mismatch '("adb" "-s" "tv.example:5555" "shell") args :test #'equal))
        collect (nthcdr 4 args)))

(define-test tv-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Tv(~a)"
               '(("Turn on the TV" "On") ("turn on the T V" "On") ("Turn off the T.V." "Off")
                 ("pause T. V." "Pause")
                 ("switch the television on" "On") ("tv on" "On")
                 ("put the tv on" "On") ("start the tv" "On") ("fire up the tv" "On")
                 ("watch tv" "On") ("let's watch some tv" "On") ("I want to watch TV" "On")
                 ("Turn off the TV." "Off") ("TV off" "Off") ("shut down the tv" "Off")
                 ("kill the tv" "Off") ("switch off the television" "Off")
                 ("Turn up the volume on the TV" "VolumeUp") ("turn the tv up" "VolumeUp")
                 ("Make the TV louder" "VolumeUp") ("increase the tv volume" "VolumeUp")
                 ("raise the volume on the tv" "VolumeUp") ("louder on the tv" "VolumeUp")
                 ("the tv is too quiet" "VolumeUp") ("I can't hear the TV" "VolumeUp")
                 ("turn the volume down on the tv" "VolumeDown")
                 ("turn down the tv" "VolumeDown") ("make the tv quieter" "VolumeDown")
                 ("lower the tv volume" "VolumeDown") ("the tv is too loud" "VolumeDown")
                 ("decrease the volume of the tv" "VolumeDown")
                 ("quieter on the tv" "VolumeDown")
                 ("Mute the TV" "Mute") ("mute tv" "Mute") ("tv mute" "Mute")
                 ("silence the tv" "Mute")
                 ("unmute the tv" "Unmute") ("tv unmute" "Unmute") ("un mute the tv" "Unmute")
                 ("Pause the TV" "Pause") ("post TV" "Pause") ("pause the movie on the tv" "Pause")
                 ("freeze the tv" "Pause") ("pause netflix" "Pause") ("pause on the tv" "Pause")
                 ("resume the tv" "Play") ("continue the show on the tv" "Play")
                 ("play TV" "Play") ("unpause TV." "Play") ("resume netflix" "Play")
                 ("continue watching" "Play")
                 ("next episode on the tv" "Next") ("skip this one on the tv" "Next")
                 ("tv next" "Next") ("next on the tv" "Next")
                 ("previous episode on the tv" "Previous") ("previous on the tv" "Previous")
                 ("tv previous" "Previous")))
    (parses-as "Tv(VolumeSet(~d))"
               '(("Set the TV volume to 30 percent" 30) ("tv volume 40" 40)
                 ("set the volume on the tv to 15" 15) ("tv volume to 20" 20)
                 ("set the tv to 35 percent" 35) ("volume 30 on the tv" 30)))
    (parses-not "Tv"
                '("turn up the volume" "pause the music" "open the browser" "play hey jude"
                  "buy a new television" "mute" "open the settings" "next song" "unpause"
                  "pause" "louder" "next" "stop" "play netflix" "open files"))))

(define-test tv-follow-ups-stay-on-the-tv
  :parent va-tests
  (with-focus (:previous (va::parse-text "pause the tv"))
    (parses-as "Tv(~a)"
               '(("unpause." "Play") ("resume" "Play") ("play" "Play") ("pause" "Pause")
                 ("louder" "VolumeUp") ("turn it up" "VolumeUp") ("quieter" "VolumeDown")
                 ("turn it down" "VolumeDown") ("mute" "Mute") ("unmute" "Unmute")
                 ("next" "Next") ("next episode" "Next") ("skip" "Next")
                 ("previous" "Previous") ("turn it off" "Off")))
    (parses-not "Tv" '("stop" "pause the music" "next song")))
  (with-focus (:previous (va::parse-text "pause the music"))
    (parses-not "Tv" '("unpause" "louder" "mute" "next" "turn it off"))))

(define-test tv-apps
  :parent va-tests
  (with-focus ()
    (flet ((app (text) (let ((a (va::parse-text text)))
                         (and a (eq (va::command-name (va::action-command a)) 'va::tv-app)
                              (second (va::tv-app-at (first (va::action-fields a))))))))
      (loop for (text want) in '(("Open Netflix on the TV" "Netflix")
                                 ("put on youtube on the tv" "YouTube")
                                 ("put netflix on the tv" "Netflix")
                                 ("play netflix on the tv" "Netflix")
                                 ("start disney plus on the television" "Disney+")
                                 ("watch netflix" "Netflix") ("watch arte" "Arte")
                                 ("launch the ard mediathek on the tv" "ARD Mediathek")
                                 ("open netflix" "Netflix") ("start youtube" "YouTube")
                                 ("put on netflix" "Netflix")
                                 ("netflix on the tv" "Netflix")
                                 ("I want to watch Netflix" "Netflix")
                                 ("let's watch some arte" "Arte")
                                 ("switch the tv to netflix" "Netflix")
                                 ("open the settings on the tv" "the TV settings")
                                 ("open the browser on the tv" "the TV browser"))
            do (is equal want (app text) "~s" text)))))

(define-test tv-sends-keys
  :parent va-tests
  (with-focus ()
    (with-tv ()
      (is equal '(("timeout" "3" "adb" "connect" "tv.example:5555")
                  ("timeout" "10" "adb" "-s" "tv.example:5555" "shell" "input" "keyevent"
                   "KEYCODE_SLEEP"))
          (va:fake-effects (va::perform (va::parse-text "turn off the tv"))))
      (loop for (text . want) in
            '(("turn on the tv" "input" "keyevent" "KEYCODE_WAKEUP")
              ("pause the tv" "input" "keyevent" "KEYCODE_MEDIA_PAUSE")
              ("resume the tv" "input" "keyevent" "KEYCODE_MEDIA_PLAY")
              ("next episode on the tv" "input" "keyevent" "KEYCODE_MEDIA_NEXT")
              ("previous episode on the tv" "input" "keyevent" "KEYCODE_MEDIA_PREVIOUS")
              ("make the tv louder" "input" "keyevent" "KEYCODE_VOLUME_UP" "KEYCODE_VOLUME_UP"
               "KEYCODE_VOLUME_UP" "KEYCODE_VOLUME_UP" "KEYCODE_VOLUME_UP")
              ("open netflix on the tv" "am" "start" "-n" "com.netflix.ninja/.MainActivity"))
            do (is equal (list want) (tv-ran text) "~s" text)))
    (with-tv ("10.0.0.7:5556")
      (is equal '("timeout" "3" "adb" "connect" "10.0.0.7:5556")
          (first (va:fake-effects (va::perform (va::parse-text "pause the tv"))))))))

(define-test tv-volume-steps-from-the-current-one
  :parent va-tests
  (with-focus ()
    (with-tv ()
      (let ((va::*fake-output* '(("timeout" . "[V] volume is 25 in range [0..100]"))))
        (is equal '("input" "keyevent" "KEYCODE_VOLUME_UP" "KEYCODE_VOLUME_UP"
                    "KEYCODE_VOLUME_UP")
            (car (last (tv-ran "set the tv volume to 28"))))
        (is equal '("input" "keyevent" "KEYCODE_VOLUME_DOWN" "KEYCODE_VOLUME_DOWN")
            (car (last (tv-ran "tv volume 23"))))
        (is equal 1 (length (tv-ran "tv volume 25")))))))

(define-test tv-mute-is-not-a-toggle
  :parent va-tests
  (with-focus ()
    (with-tv ()
      (flet ((keys (text muted)
               (let ((va::*fake-output*
                      `(("timeout" . ,(format nil "- STREAM_MUSIC:~%   Muted: ~a~%" muted)))))
                 (rest (tv-ran text)))))
        (is equal '(("input" "keyevent" "KEYCODE_VOLUME_MUTE")) (keys "mute the tv" "false"))
        (is equal nil (keys "mute the tv" "true"))
        (is equal '(("input" "keyevent" "KEYCODE_VOLUME_MUTE")) (keys "unmute the tv" "true"))
        (is equal nil (keys "unmute the tv" "false"))))))

(define-test tv-needs-an-address
  :parent va-tests
  (with-focus ()
    (let ((va::*tv-file* #p"/nonexistent/tv"))
      (false (va:fake-effects
              (true (search "/nonexistent/tv"
                            (va::reply-body (va::perform (va::parse-text "pause the tv"))))))))))

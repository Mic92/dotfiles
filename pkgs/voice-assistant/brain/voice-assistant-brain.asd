;;; The voice assistant's brain: grammar, skills and dialogue.

(defsystem "voice-assistant-brain"
    :depends-on ("coalton" "alexandria" "trivia" "com.inuoe.jzon" "sb-bsd-sockets" "sb-introspect")
    :serial t
    :pathname "src/"
    :components #.`(,@(mapcar (lambda (f) (list :file f))
                              '("package" "defn" "strings" "json" "effects" "replies" "types"
                                "adt" "words" "duration" "clock" "date" "holiday" "grammar"
                                "commands" "dispatch" "dialogue" "fuzzy" "dev"))
                      (:module "skills" :pathname "../skills/" :serial t
                               ;; In Rust's MODULES order, which breaks ties between commands.
                               :components ,(mapcar (lambda (f) (list :file f))
                                                    '("timer" "system" "volume" "media" "desktop" "apps"
                                                      "info" "fun" "news" "kagi" "core")))))

(defsystem "voice-assistant-brain/tests"
    :depends-on ("voice-assistant-brain" "parachute" "cl-ppcre")
    :pathname "tests/"
    :serial t
    :components #.(mapcar (lambda (f) (list :file f))
                          '("tests" "parsing" "grammar" "dispatch" "fun" "timer" "dialogue"
                            "system" "media" "desktop" "info" "kagi" "news" "types" "source"))
    :perform (test-op (o c) (symbol-call :parachute :test :va-tests)))

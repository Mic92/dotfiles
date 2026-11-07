;;; The voice assistant's brain: grammar, skills and dialogue.

(defsystem "voice-assistant-brain"
    :depends-on ("coalton" "alexandria" "trivia" "com.inuoe.jzon" "sb-bsd-sockets" "sb-introspect"
                           "cl-ppcre")
    :serial t
    :pathname "src/"
    :components #.`(,@(mapcar (lambda (f) (list :file f))
                              '("package" "defn" "strings" "json" "effects" "replies" "types" "adt"
                                "words" "duration" "clock" "date" "holiday" "grammar" "commands"
                                "dispatch" "dialogue" "fuzzy" "protocol" "conversation" "dev"))
                      (:module "skills" :pathname "../skills/" :serial t
                               ;; In Rust's MODULES order, which breaks ties between commands.
                               :components ,(mapcar (lambda (f) (list :file f))
                                                    '("timer" "system" "volume" "media" "desktop"
                                                      "apps" "info" "calc" "spell" "fun" "chat"
                                                      "ops" "notes" "reminders" "meetings" "news"
                                                      "kagi" "core")))))

(defsystem "voice-assistant-brain/tests"
    :depends-on ("voice-assistant-brain" "parachute")
    :pathname "tests/"
    :serial t
    :components #.(mapcar (lambda (f) (list :file f))
                          '("tests" "parsing" "grammar" "dispatch" "fun" "timer" "dialogue"
                            "system" "media" "desktop" "info" "kagi" "news" "spell" "calc" "chat"
                            "ops" "notes" "reminders" "meetings" "protocol" "conversation" "types"
                            "source"))
    :perform (test-op (o c) (symbol-call :parachute :test :va-tests)))

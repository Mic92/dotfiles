# Plan: move the voice assistant's brain to SBCL

## Why

A Rust rebuild takes too long for what we change most often: templates, replies
and skills. Those parts are plain code, not hot paths. In Lisp they can be
reloaded into the running assistant in milliseconds.

## SBCL answers

- **Hot reloading:** yes, it's built in. A running image can recompile a
  function, a file or a whole ASDF system. Calls pick up the new definition
  right away and state such as timers or the open question survives. Redefined
  classes update live objects (`update-instance-for-redefined-class`).
- **Type checking:** partial but useful. SBCL infers types and warns at compile
  time on contradictions when given `(declaim (ftype ...))`, `deftype` and
  `check-type`. That catches wrong argument types, arity errors, undefined
  functions and unused variables. It is not a full static type system and won't
  prove exhaustive matches or nil-safety. If we want that, **Coalton** (ML-style
  static types inside Common Lisp) can be used for the grammar and data types
  only. Default: plain CL with ftype declarations, plus warnings promoted to
  errors in CI.
- **Reloading for agents:** the brain opens a Unix socket
  (`$XDG_RUNTIME_DIR/voice-assistant/repl.sock`). A small CLI, `va-eval`, sends
  a form and prints the result, the output and any compiler warnings. For
  example:
  - `va-eval '(va:reload)'` recompiles changed files and reports warnings.
  - `va-eval '(va:parse "tell a joke")'` gives the same as `--parse` today.
  - `va-eval '(va:test)'` runs the test suite inside the image. Slynk/Swank can
    also run for Emacs/Neovim, but agents get the simple socket.

## Split

**Stays in Rust (the ears and mouth, perf or FFI heavy):** pipewire capture,
wake word, VAD, parakeet ASR, acoustic rescoring, the intent classifier (ort),
TTS, the phoneme lexicon (espeak trie, 0.3 ms) and hush/volume ducking.

**Moves to Lisp (the brain, about 12k of 18.7k lines):** grammar, `skills/*`
(calc, chat, meetings, notes, reminders, timer, media, info, ops, fun, ...),
dialogue/followup, date/duration/holiday, `say`/pick, store and replay logic.

## Boundary

Two processes over a Unix socket using line-delimited JSON. Rust is the
long-running audio daemon and Lisp is the brain. Each can restart without the
other, and the brain can be reloaded without touching audio.

Messages:

| From        | Message                                                                                                   |
| ----------- | --------------------------------------------------------------------------------------------------------- |
| Rust → Lisp | `heard {text, n_best, confidence}`                                                                        |
| Lisp → Rust | `reply {speech, notify, actions}` or `miss`                                                               |
| Lisp → Rust | `candidates?` → Rust's phoneme lexicon returns sound-alikes, Rust rescores them, then sends `heard` again |
| Rust → Lisp | `classify` results, when the grammar misses                                                               |
| Lisp → Rust | `lexicon {sentences}` on startup and after reload, so sound-alikes follow the current grammar             |
| Lisp → Rust | `say`, `chime`, `duck` for timers/reminders that fire on their own                                        |

Alternatives considered: SBCL loading Rust as a `cdylib` via CFFI (one process
but a crash takes everything down and reloads are riskier), and embedding
libsbcl in Rust (awkward build, little gain).

## Decisions

- **Boundary:** two processes, JSON over a Unix socket.
- **Types:** Coalton for the core data (`Action`, slot values, dates, dialogue
  state), plain CL with `ftype` for skills. Warnings are errors. Coalton's
  source is pinned via nix so agents can read it.
- **Reload:** `va-eval` always, Swank on demand for Neovim.
- **Tests:** parachute.
- **Order:** port the whole brain, then switch over once.
- **Actions:** `defcommand` generates a Coalton ADT constructor, so dispatch is
  checked to cover every command.
- **Replies:** CL `format` directives (`~a`, `~:d`, `~p` for plurals) instead of
  `{}`.
- **Examples:** inline in `defcommand` (`:examples`), checked by one parachute
  test over the registry. `tests/golden.tsv` stays separate as generated data.
  Test sentences stay as people actually say them, crude ones included.

## Abstractions: what gets shorter

The Rust brain spells out the same mechanics in every skill. In Lisp, each
becomes a macro or a table.

**1. Skill = enum + `intent(...)` closures + `impl Skill` + dispatch match.**
Today a skill touches four places: the enum, the `intents()` list, the `run`
match and `Action::skill`. One `defcommand` replaces all of that:

```lisp
(defcommand joke
  "[please] (tell|give) [me|us] [a|another] [funny|bad|good|short] joke"
  "(make|cheer) (me|us) (laugh|up)"
  "![another|one more] joke"
  :filler *joke-intros*
  :repeatable t
  (say (or (online-joke) (pick *jokes*))))

(defcommand dice
  "roll a (die|dice|d6)" "roll a {number :range (2 1000) :default 6} sided (die|dice)"
  (let ((roll (1+ (random number))))
    (say (cond ((= roll 1) "A one. Ouch. Critical fail.")
               ((= roll number) "~a! Natural max, baby!")
               (t "You rolled a ~a."))
         roll)))
```

Slots are bound as variables, and range checks and defaults sit in the template,
which replaces the `match s.get("number")` and cast boilerplate. The macro
registers the templates, the sound-alike phrasings and the dispatch, and creates
the Coalton `Action` constructor.

**2. Reply tables, late variants and `pick`.** `const BODY_LATE: &[&str]` plus
`if ctx.late()` turns into:

```lisp
(defreplies *joke-intros*
  "Drum roll, please." "Okay. Brace yourself."
  (:late "Fine, but quietly, it's late."))
```

`(pick *joke-intros*)` picks the late lines automatically after 23:00.

**3. Closed slots.** `body_lookup`/`agent_lookup` functions and
`Module { slots }` become:

```lisp
(defslot body ("boobs" "tits" "bum" "butt") :value :text)
```

**4. Dialogue questions.** `impl Question for AskTime` structs plus
`answer_words` become one form, with follow-up answers as small templates:

```lisp
(ask "When should it ring?"
  ("{duration}" (start-timer duration))
  ("never mind" (dismiss)))
```

**5. Running programs.** Rust
`spawn("systemd-run", &["--user", "--collect", ...])` arg arrays become
`(run-later time "notify-send" "-u" "critical" "Reminder" what)` on top of
`uiop:run-program`.

**6. Tests.** Tuple lists `("Tell me a joke.", Fun::Joke)` become per-skill
tables inside the command: `(:examples "tell a joke" "joke!")`. One parachute
test checks every command's examples, and the golden file checks the rest.

**7. HTTP + JSON.** `http.rs` + tinyjson field walking becomes dexador + jzon
with a `(json-get doc "joke" "setup")` path accessor.

Goal: the brain shrinks from about 12k Rust lines to roughly a third, and adding
a command means writing one `defcommand`.

## Steps

1. **Golden fixtures from Rust:** dump `--parse` output for every test sentence,
   the CLINC set and heard.tsv into `tests/golden.tsv`. The Lisp port must match
   it line by line.
2. **Skeleton:** an ASDF system `voice-assistant-brain`, a nix package
   (`sbcl.withPackages`), the REPL socket, `va-eval`, parachute and treefmt/CI
   hooks. Then the `defcommand`/`defreplies`/`defslot`/`ask` macros, tried out
   on fun.rs first.
3. **Grammar + dispatch:** templates, slots, `best_match`, dates and durations.
   Gate: golden parse results match.
4. **Skills, ported fully before switching** (decided: no Rust fallback in
   between): fun/chat/say, timer/reminders (systemd-run), notes/todos/calendar
   (khal/todoman), media, info/kagi/news (HTTP), ops and system. Each one is
   checked against the golden fixtures.
5. **Switch over in one step:** replace the `skills::` calls in `main.rs` with
   the socket client, keep `--parse` / `--replay` working through it and delete
   the Rust brain once replay shows no regressions. Rust keeps only the audio
   side, so it rarely needs rebuilding.

## Risks

- **Two processes to keep in sync:** the protocol is versioned and replay runs
  end-to-end in CI.
- **Weaker typing than Rust:** ftype declarations, warnings as errors and golden
  tests make up most of it.
- **Startup:** an SBCL core image built by nix starts in well under a second.

# My personal voice assistant

See
https://blog.thalheim.io/2026/10/24/building-a-personal-voice-assistant-with-an-attitude-in-2026/
for details.

## Layout

- `src/` (Rust): microphone, wake word, speech recognition, TTS. Runs as
  `voice-assistant.service`.
- `brain/` (Common Lisp): understands requests and runs the skills. Runs as
  `voice-assistant-brain.service`.
  - `brain/skills/*.lisp`: one file per skill (`defcommand`, `defreplies`).
  - `brain/tests/*.lisp`: tests. New files go into `voice-assistant-brain.asd`.
- `tests/golden.tsv`: sentences and the command each must parse to.

## Making a change

1. Write a failing test in `brain/tests/`, then change `brain/skills/` or
   `brain/src/`.
2. Check:
   ```sh
   nix build ../..#voice-assistant-brain   # runs the tests
   ```
   `git add` new files first, or the flake won't see them.
3. Try it on the running brain without a restart:
   ```sh
   va-reload                               # loads this checkout into the live brain
   va-eval --live '(va::action-debug (va::parse-text "power off"))'
   ```
   `va-reload` is fine for edits. New commands need a full update to take
   priority over older catch-alls.

## Updating

```sh
nix run ~/.homesick/repos/dotfiles#hm -- switch
```

This restarts both services. The assistant keeps running while only the brain
restarts.

## Debugging

- What was heard: `~/.local/state/voice-assistant/heard.tsv`, recordings of
  misses in `misses/`.
- Wake triggers: `~/.local/state/voice-assistant/wakewords.tsv`.
- Logs: `journalctl --user -u voice-assistant -u voice-assistant-brain`.

;;;; This script can be loaded during emacs initialization to automatically
;;;; send `kill-region' and `kill-ring-save' regions to your system clipboard.
;;;; The OSC 52 terminal escape sequence is used to transfer the selection from
;;;; emacs to the host terminal.

;;;; It is based off of the osc52.el copyright the Chromium OS authors, but
;;;; was modified to add support for tmux, graphical displays, and
;;;; multi-byte strings.

;;;; It works in hterm, xterm, and other terminal emulators which support the
;;;; sequence.

;;;; It also works under screen, via `osc52-select-text-dcs' and tmux via
;;;; `osc52-select-text-tmux', as long as the terminal supports OSC 52.

;;;; Call `osc52-set-cut-function' to activate.

(defcustom osc52-max-sequence 100000
  "Maximum length of the OSC 52 sequence.

The OSC 52 sequence requires a terminator byte.  Some terminals will ignore or
mistreat a terminated sequence that is longer than a certain size, usually to
protect users from runaway sequences.

This variable allows you to tweak the maximum number of bytes that will be sent
using the OSC 52 sequence.

If you select a region larger than this size, it won't be copied to your system
clipboard.  Since clipboard data is base 64 encoded, the actual number of
characters that can be copied is 1/3 of this value.")

(defcustom osc52-multiplexer 'tmux
  "Select which terminal multiplexer should be used when creating OSC 52 sequences. Device control string escape sequences are only used when the value of the environment variable TERM starts with the string \"screen\".

If set to 'tmux, a tmux DCS escape sequence will be generated, otherwise a screen DCS will be used.")

(defun osc52-select-text (string &optional replace yank-handler)
  "Copy STRING to the system clipboard using the OSC 52 escape sequence.

Set `interprogram-cut-function' to this when using a compatible terminal, and
your system clipboard will be updated whenever you copy a region of text in
emacs.

If the resulting OSC 52 sequence would be longer than
`osc52-max-sequence', then the STRING is not sent to the system
clipboard.

This function sends a raw OSC 52 sequence and will work on a bare terminal
emulators.  It does not work on screen or tmux terminals, since they don't
natively support OSC 52."
  (let ((b64-length (+ (* (length string) 3) 2)))
    (if (<= b64-length osc52-max-sequence)
        (send-string-to-terminal
         (concat "\e]52;c;"
                 (base64-encode-string string t)
                 "\07"))
      (message \"Selection too long to send to terminal %d\" b64-length)
      (sit-for 2))))

(defun osc52-select-text-dcs (string &optional replace yank-handler)
  "Copy STRING to the system clipboard using the OSC 52 escape sequence, for
screen users.

Set `interprogram-cut-function' to this when using the screen program, and your
system clipboard will be updated whenever you copy a region of text in emacs.

If the resulting OSC 52 sequence would be longer than
`osc52-max-sequence', then the STRING is not sent to the system
clipboard.

This function wraps the OSC 52 in a Device Control String sequence.  This causes
screen to pass the wrapped OSC 52 sequence along to the host termianl.  This
function also chops long DCS sequences into multiple smaller ones to avoid
hitting screen's max DCS length."
  (let ((b64-length (+ (* (length string) 3) 2)))
    (if (<= b64-length osc52-max-sequence)
        (send-string-to-terminal
         (concat "\eP\e]52;c;"
                 (replace-regexp-in-string
                  "\n" "\e\\\\\eP"
                  (base64-encode-string (encode-coding-string string 'binary)))
                 "\07\e\\"))
        (message "Selection too long to send to terminal %d" b64-length)
        (sit-for 2))))

(defun osc52-select-text-tmux (string &optional replace yank-handler)
  "Copy STRING to the system clipboard using the OSC 52 escape sequence, for
tmux users.

Set `interprogram-cut-function' to this when using the screen program, and your
system clipboard will be updated whenever you copy a region of text in emacs.

If the resulting OSC 52 sequence would be longer than
`osc52-max-sequence', then the STRING is not sent to the system
clipboard.

This function wraps the OSC 52 in a Device Control String sequence.  This causes
screen to pass the wrapped OSC 52 sequence along to the host termianl.  This
function also chops long DCS sequences into multiple smaller ones to avoid
hitting screen's max DCS length."
  (let ((b64-length (+ (* (length string) 3) 2)))
    (if (<= b64-length osc52-max-sequence)
        (send-string-to-terminal
         (concat "\ePtmux;\e\e]52;c;"
                 (base64-encode-string (encode-coding-string string 'binary)
                                       t)
                 "\a\e\\"))
      (message "Selection too long to send to terminal %d" b64-length)
      (sit-for 2))))

(defvar osc52-cut-function)

(defun osc52-interprogram-cut-function (string &optional replace yank-handler)
  (if (display-graphic-p)
      (x-select-text string)
    (funcall osc52-cut-function string)))

(defun osc52-set-cut-function ()
  "Initialize the `interprogram-cut-function' based on the value of
`display-graphic-p' and the TERM environment variable."
  (interactive)

  ;; Look `initial-environment' instead of `(getenv "TERM")',
  ;; because emacs might set it to "dumb" internally.
  ;; `inital-environment' has the pure value when it started.
  (let ((term
	 ;; Make term == "" instead of nil, when no TERM environment variable
	 (or (ignore-errors
	       (replace-regexp-in-string
		"^TERM=" ""
		(dolist (env initial-environment)
                  (if (string-match "^TERM=" env) (return env)))
		'fixedcase))
	     "")))

    (setq osc52-cut-function
          ;; If `TERM' contains "tmux", they should use tmux.
          (if (string-match "tmux" term)
              'osc52-select-text-tmux

            ;; Otherwise, if the `TERM' starts from "screen",
            ;; they might actually use screen, but perhaps tmux.
            ;; They have to set actual terminal by `osc52-multiplexer'
            (if (string-match "^screen" term)
                (if (equal osc52-multiplexer 'tmux)
                    'osc52-select-text-tmux
                  'osc52-select-text-dcs)

              ;; No terminal multiplexer.
              'osc52-select-text))))

  (setq interprogram-cut-function 'osc52-interprogram-cut-function))

(defun osc52-send-region-to-clipboard (START END)
  "Copy the region to the system clipboard using the OSC 52 escape sequence."
  (interactive "r")
  (osc52-interprogram-cut-function (buffer-substring-no-properties
                           START END)))

(provide 'osc52e)

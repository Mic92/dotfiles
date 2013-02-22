; -*-mode: Emacs-Lisp-*-
;; Mic92's .emacs for GNU/Emacs 24.0.50.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{{ Initialization
;; Define the load path and add everything recursively
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))
;; Start server
(server-start)
;; }}}

;; {{{ Look & Feel
;; Default font
(defun font-exists (font)
  "Return font if font exits"
    (if (null (x-list-fonts font))
	nil font))
;; fallback if a font does not exits, works only in a window system
(when window-system (set-frame-font (cond ((font-exists "Dejavu Sans Mono 11"))
 					  ((font-exists "Monaco 10"))
 					  ((font-exists "sans")))))
(setq font-use-system-font t)

;; Color theme
;(load "~/.emacs.d/zenburn-theme")
;(load "~/.emacs.d/naquadah-theme")
;(load-theme 'solarized-dark t)


;; Don't show the welcome message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Shut off message buffer
(setq message-log-max nil)
(kill-buffer "*Messages*")

;; Syntax coloring (font-lock-mode)
(global-font-lock-mode t)

;; Always flash for parens and define a more distinctive color
(show-paren-mode 1)
(set-face-foreground 'show-paren-match-face "#bc8383")

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Modeline setup
(column-number-mode t)

;; include the host (system) name and indication of the status of this buffer
(setq frame-title-format
  '("emacs@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
		(abbreviate-file-name (buffer-file-name))
		  "%b")) " [%*]"))
;; }}}

;; {{{ ELPA - Packagemanagement
(load "package")
(package-initialize)
;; Add the user-contributed repository
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu"  . "http://elpa.gnu.org/packages/")
			 ("SC"   . "http://joseito.republika.pl/sunrise-commander/")))
;; }}}

;; {{{ General

;; {{{ Encoding
;; - Hopefully everthing utf-8
(setq read-quoted-char-radix 10    ; use decimal, not octal
      locale-coding-system 'utf-8
      x-select-request-type '(COMPOUND_TEXT UTF8_STRING STRING))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Copy/paste with accentuation intact
(setq selection-coding-system 'compound-text-with-extensions)
;(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")  ; prefer utf-8 for language settings
(prefer-coding-system 'utf-8)
;; to setup tabs
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)
;; }}}

;; {{{ Default Web Browser
(setq browse-url-browser-function 'browse-url-firefox ;; Default is firefox
      browse-url-new-window-flag  t                   ;; don't overwrite current window
      browse-url-firefox-new-window-is-tab t)         ;; open a new tab instead of a window

;;To bind the browse-url commands to keys with the `C-c u' prefix:
(global-set-key "\C-cu" 'browse-url-at-point)

;; display a URL by shift-clicking on it
(global-set-key [S-mouse-2] 'browse-url-at-mouse)
;; (Note that using Shift-mouse-1 is not desirable because
;; that event has a standard meaning in Emacs.)

(autoload 'browse-url "browse-url" "browse-url loaded!" t)
;; }}}

;; Show unfinished keystrokes early
(setq echo-keystrokes 0.1)

;; Selection mode with S-ENTER
(cua-selection-mode t)

;; TAB first tries to indent the current line, and if the line
;; was already indented, then try to complete the thing at point.
(setq tab-always-indent 'complete)

;; rotating the kill ring with M-y (yank-pop) also saves the new yank to the primary selection
(setq yank-pop-change-selection t)

;; each kill command first saves the existing selection onto the kill ring
(setq save-interprogram-paste-before-kill t)

;; Save aftnner a certain amount of time.
(setq auto-save-timeout 1800)

;; {{{ Backup
;; Change backup behavior to save in a specified directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      backup-by-copying      t
      version-control        t
      delete-old-versions    t
      kept-new-versions      6
      kept-old-versions      2
)
;; }}}

;; Keep bookmarks in the load path
(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1) ;; autsave each change

;; Keep abbreviations in the load path
(setq abbrev-file-name "~/.emacs.d/abbrev-defs"
      abbrev-mode t   ;; turn it on
      save-abbrevs t) ;; don't ask on saving
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file)) ;; don't tell
(add-hook 'kill-emacs-hook    ;; write when ..
	  'write-abbrev-file) ;; ... exiting emacs

;; Narrowing enabled
(put 'narrow-to-region 'disabled nil)
;; }}}

;; {{{ Mouse and cursor settings

;; Enable mouse scrolling
(mouse-wheel-mode t)

;; Accelerate the cursor when scrolling
(load "accel" t t)

;; Always paste at the cursor
(setq mouse-yank-at-point t)

;; Cursor Theme
(setq-default cursor-type '(hbar . 2))

;; Higline current line
(global-hl-line-mode t)

;; Placing the cursor
(require 'centered-cursor-mode)
(global-centered-cursor-mode t)

;; Kill (and paste) text from read-only buffers
(setq kill-read-only-ok 1)

;; kills an entire line if the cursor is at the beginning of line
(setq kill-whole-line t)

;; Partially integrate the kill-ring and X cut-buffer
(setq x-select-enable-clipboard t)
;; }}}

;; {{{ Settings for modes
;; - major modes for editing code and other formats are defined below

(setq major-mode 'indented-text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; {{{ AUCTeX
;; - http://www.gnu.org/software/auctex/documentation.html
(load "auctex.el" 'noerror)
(load "preview-latex.el" 'noerror)

(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (turn-on-auto-fill) ;; Completion
	     (turn-on-reftex)    ;; Load reftex
	     (TeX-fold-mode 1)   ;; Folding Macros and Environments
	     (LaTeX-math-mode)
	     (tex-pdf-mode)      ;; PDF as default output format
	     (tex-source-correlate-mode) ;; Source Special for pdflatex
	     (flyspell-mode)     ;; On-the-Fly-Spell checking using aspell
	     (define-key LaTeX-mode-map [f8] 'tex-math-preview)
	     (local-set-key "," (lambda nil (interactive) (insert ", ")))
	     (local-set-key ";" (lambda nil (interactive) (insert "; ")))
	     (local-set-key ":" (lambda nil (interactive) (insert ": ")))
	     ))

(setq TeX-newline-function 'newline-and-indent ;; indent automatic on newline
      reftex-plug-into-AUCTeX t                ;; suggestions for section labels
      LaTeX-default-format "X|X"               ;; default tabularx format
      TeX-electric-sub-and-superscript 1       ;; insert braces after typing <^> and <_>
      TeX-electric-escape t                    ;; bind "/" to TeX-electric-macro
      TeX-insert-braces nil                    ;; don't add brackets after macro
      LaTeX-math-abbrev-prefix "#"
      LaTeX-math-list (append '((?f "frac" nil) ))
      )

;; make AUCTeX aware of style files and multi-file documents
(setq TeX-auto-save t
      TeX-parse-self t)
(setq-default TeX-master nil)

(autoload 'whizzytex-mode "whizzytex" "WhizzyTeX, a minor-mode WYSIWIG environment for LaTeX" t)

;; Guess the language
;; - http://nschum.de/src/emacs/auto-dictionary/
(setq ispell-program-name "aspell")
(require 'auto-dictionary)
(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))
;; Deutsche Rechtschreibung falls \usepackage{ngerman}
;; oder german benutzt wird
(add-hook 'TeX-language-de-hook
	  (function (lambda () (ispell-change-dictionary "german8"))))
;; }}}

;; {{{ Auto Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; }}}

;; {{{ Auto Compression
;; - edit files in compressed archives
(auto-compression-mode t)
;; }}}

;; {{{ Autopair
;; - http://www.emacswiki.org/emacs/AutoPairs
;; - It can be useful to insert parentheses, braces, quotes and the
;;   like in matching pairs
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers
(setq autopair-autowrap t)
;; }}}

;; {{{ Browse-kill-ring
;; - open a buffer which lists the contents of the kill ring
;; - activate with M-y
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
;; }}}

;; {{{ CC mode
;;  "gnu": The default indention style for GNU projects
(setq c-default-style "java")
;; auto indention - love it or hate it
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1) ))
;; }}}

;; {{{ Csharp
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; }}}

;; {{{ CPerl mode
;; - cperl-mode is preferred to perl-mode
;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
	  (lambda()
	    (eval-when-compile (require 'cperl-mode))
	    (linum-mode 1)                ;; show line numbers
	    (yas/minor-mode-on)
	    (setq
	     cperl-hairy nil              ;; parse hairy perl constructs
	     abbrev-mode t
	     cperl-auto-newline t
	     cperl-auto-newline-after-colon t
	     cperl-invalid-face nil       ;; don't show underlines
	     cperl-electric-linefeed t
	     cperl-electric-keywords t    ;; complete keywords
	     cperl-electric-parens nil )));; already done by autopair

;; this fixes the problem that most color themes have with cperl-mode.
(defvar cperl-array-face)               ;; tell compiler not to warn
					;; about this varible
(add-hook 'cperl-mode-hook (lambda () (set-face-background
				       cperl-array-face nil)))
;; }}}

;; {{{ Crontab mode
;; - http://www.mahalito.net/~harley/elisp/crontab-mode.el
(autoload 'crontab-mode "~/.emacs.d/crontab-mode.el" "Major mode for editing the crontab" t)
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\." . crontab-mode))
;; }}}

;; {{{ Dired
(require 'dired-x)
(eval-after-load "dired" '(require 'dired+))
(eval-after-load "dired-x" '(require 'dired+))

(setq dired-omit-files	(concat dired-omit-files "\\|^\\..+$")	    ;; omit dot filesx
      dired-recursive-deletes 'always
      dired-recursive-copies 'always)


(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-omit-mode 1)
	    (require 'find-dired)
	    (toggle-dired-find-file-reuse-dir 1)
	    ))

(setq delete-by-moving-to-trash t)
;; Automatic space after a comma
(global-set-key "\C-x\C-j" 'dired-jump)

;; }}}

;; {{{ DeskTop - Session management
;; - Loading files from a previous session at startup#
(desktop-save-mode 1)
;; autosave
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
;; don't ask when closing
(desktop-lazy-abort)

;; use only one desktop
(setq desktop-path '("~/.emacs.d/")
      desktop-dirname "~/.emacs.d/"
      desktop-base-file-name "DeskTop")

;; specify buffers which should not be saved
(setq desktop-buffers-not-to-save
      '("\(" "^nn\.a[0-9]+\|\.log\|(ftp)\|^tags\|^TAGS\|\.diary\|\.newsrc-dribble\|\.bbdb\)$"
	;; Info-mode
	'info-lookup-mode
	'fundamental-mode
	'dired-mode
	))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
		(file-name-history        . 100)
		(grep-history             . 30)
		(compile-history          . 30)
		(minibuffer-history       . 50)
		(query-replace-history    . 60)
		(read-expression-history  . 60)
		(regexp-history           . 60)
		(regexp-search-ring       . 20)
		(search-ring              . 20)
		(shell-command-history    . 50)
		tags-file-name
		register-alist)))
;; }}}

;; {{{ Ediff
;; Don't spawn a new frame and split the frame horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)
;; }}}

;; {{{ Elisp
;; Elisp
(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (modify-syntax-entry ?- "w")   ; now '-' is not considered a word-delimiter
    (font-lock-add-keywords nil '(("^[^\n]\\{80\\}\\(.*\\)$"
				    1 font-lock-warning-face prepend)))
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\)"
				    1 font-lock-warning-face prepend)))
    (font-lock-add-keywords nil
			    '(("\\<\\(add-hook\\|setq\\|autoload\\|add-to-list\\)"
			       1 font-lock-keyword-face prepend)))))
;; }}}


(load-theme 'solarized-light t)
;(load-theme 'solarized-dark t)


;; {{{ Fsharp
(setq load-path (cons "~/.emacs.d/fsharp" load-path))
(setq auto-mode-alist (cons '("\\.fs[iylx]?$" . fsharp-mode) auto-mode-alist))
(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

(setq inferior-fsharp-program "fsharpi")
(setq fsharp-compiler "fsharpc")
;; }}}

;; {{{ Go
;(require 'go-mode-load)
;; }}}

;; {{{ Ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(eval-after-load "ibuffer-ext"
  (setq ibuffer-saved-filter-groups
	       (quote (("default"
		   ("dired" (mode . dired-mode))
		   ("emacs" (or
			     (name . "^\\*scratch\\*$")
			     (name . "^\\*Messages\\*$")))
		   ("Documenting" (mode . latex-mode))
		   ("Configuration"
		    (or
		     (filename . ".emacs.d")
		     (filename . ".emacs")
		     (filename . ".config")))
		   ("Programming"
		    (or
		     (mode . c-mode)
		     (mode . c++-mode)
		     (mode . perl-mode)
		     (mode . python-mode)
		     (mode . sh-mode)
		     (mode . emacs-lisp-mode)))
		   ("Help" (or (name . "\*Help\*")
			       (name . "\*Apropos\*")
			       (name . "\*info\*")
			       )))))
	       ibuffer-show-empty-filter-groups nil
	       ;; Close modified buffer without asking
	       ibuffer-expert t))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;; eleminate the need of C-xo to switch tof the buffer-menu
(global-set-key (kbd "\C-x\C-b") 'ibuffer)
;; }}}

;; {{{ Icomplete
;;   - completion in the mini-buffer
(icomplete-mode t)
;; }}}

;; {{{ IDO
;; ido makes competing buffers and finding files easier
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq
 ido-create-new-buffer 'prompt
 ido-save-directory-list-file "~/.emacs.d/ido.last"
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop"
			   "~/Dropbox/Dokumente"
			   "~/.config/awesome"
			   "~/.emacs.d"
			    "~/git")
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-enable-flex-matching    t    ; match any item containing the entered characters
 ido-case-fold               t    ; case insensitive
 ido-use-virtual-buffers     t    ; complete recent files as well
 ido-use-url-at-point        t    ;
)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)
(setq smex-save-file "~/.emacs.d/smex.save") ;; keep my ~/ clean
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; }}}

;; {{{ Lua mode
;; - http://lua-mode.luaforge.net/
(autoload 'lua-mode "lua-mode" "Major-mode for editing lua scripts." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-hook 'lua-mode-hook 'hs-minor-mode)
;; }}}

;; {{{ OrgMode
;; - http://www.emacswiki.org/emacs/OrgMode
;; Initialization
(require 'org-install)
;; Settings
(add-to-list 'auto-mode-alist '("\\.org$'" . org-mode))
(setq
 org-directory "~/.org/"
 ;; Files that are included in org-mode agenda
 org-agenda-files
 (list "~/.org/index.org" "~/.org/work.org" "~/.org/personal.org" "~/.org/computers.org")
 org-return-follows-link t
 org-completion-use-ido t
 ;; Add a time after a task is done
 org-log-done 'time)

;; org-mode bindings for quick access (see below)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)
;; }}}

;; {{{ Remember mode
;; - connected with org-mode
;; Initialization
(autoload 'remember "remember" nil t)
;; Connect with org-mode
(eval-after-load "remember" (org-remember-insinuate))
;; Notes file
(setq org-default-notes-file (concat org-directory "notes.org"))
;; }}}

;; {{{ Saveplace
;; - places cursor in the last place you edited file
;; activate it for all buffers
(setq-default save-place t)
;; Keep places in the load path
(setq save-place-file "~/.emacs.d/saveplace")
;; }}}

;; {{{ Smart Operators
(add-hook 'c-mode-common-hook
	  (lambda()
	    (require 'smart-operator)
	    (smart-insert-operator-hook)))
;; }}}

;; {{{ PARedit
	(defun paredit-replace-autopair ()
  "Enable paredit in current buffer, while disabling autopair"
  (paredit-mode +1)
  (setq autopair-dont-activate t))
(add-hook 'emacs-lisp-mode-hook 'paredit-replace-autopair)
(add-hook 'lisp-mode-hook 'paredit-replace-autopair)
(add-hook 'lisp-interaction-mode-hook 'paredit-replace-autopair)
;; }}}

;; {{{ PKGBUILD
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))
;; }}}

;; {{{ Python
(add-hook 'python-mode-hook
	  #'(lambda ()
	      (setq autopair-handle-action-fns
		    (list #'autopair-default-handle-action
			  #'autopair-python-triple-quote-action))))
;; }}}

;; {{{ Speedbar
(require 'speedbar)
;; Additional extensions we are interested in
(speedbar-add-supported-extension  '("PKGBUILD" ".txt" ".org" ".xml" ".pdf" ".patch"
				     ".diff" ".php" ".py"  ".lua" ".sh" ".pl" ".tex" ".el" ".h" ".c" ".cpp" ".java" ))
;; Quick access to the speedbar
(global-set-key "\C-cs" 'speedbar-get-focus)
;; }}}

;; {{{ Tramp mode
;; - transparent remote access
(autoload 'tramp "tramp" "Remotely access files." t)
;; make it working with zsh
(eval-after-load "tramp"
    (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
	  tramp-defpault-method "sudo"))
;; }}}

;; {{{ Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
;; }}}

;; {{{ Winner Mode
(winner-mode 1)
;; ‘S-right’, ‘S-left’, ‘S-up’, ‘S-down’
(windmove-default-keybindings)
;; }}}

;; {{{ Yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet/")
(require 'yas-jit)
(setq yas/root-directory "~/.emacs.d/yasnippet/snippets"
      yas/trigger-key "<C-tab>")
(yas/jit-load)
(yas/global-mode)
;; }}}

;; }}}

;; {{{ Own function

;; {{{ Edit .emacs on the fly
;; - key bindings defined below
(defun aic-edit-dot-emacs ()
  "Edit user configuration in .emacs"
  (interactive)
  (find-file "~/.emacs"))
;; Edit .emacs as defined above
(global-set-key "\C-ce" 'aic-edit-dot-emacs)
;; }}}

;; {{{ Quick access to OrgMode and the OrgMode agenda
;; - org-mode configuration defined below
(defun aic-org-index ()
   "Show the main org file."
   (interactive)
   (find-file "~/.org/index.org"))
(defun aic-org-agenda ()
  "Show the org-mode agenda."
  (interactive)
  (call-interactively 'org-agenda-list))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "ctags -e -R %s" dir-name)))
;; }}}

;; {{{ Desktop-override-stale-locks
;; - Automatically Overriding Stale Locks from desktop mode
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
	(with-temp-buffer
	  (insert-file-contents-literally cmdline-file)
	  (goto-char (point-min))
	  (search-forward "emacs" nil t)
	  pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;; }}}

;; }}}

;; {{{ Key bindings

;; Require C-x C-c prompt, no accidental quits
(global-set-key [(control x) (control c)]
  (function
    (lambda () (interactive)
     (cond ((y-or-n-p "Quit? ")
       (save-buffers-kill-emacs))))))
;; }}}

;; {{{ Custom-setvariables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "" "evince %o") ("^pdf$" "." "xpdf -remote %s -raise %o %(outpage)") ("^html?$" "." "netscape %o"))))
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "6e2042742f50be15ee96f86099ca703d933332b8" default)))
 '(dired-recursive-deletes (quote top))
 '(doc-view-continuous t)
 '(frame-background-mode (quote dark))
 '(gdb-many-windows t)
 '(mouse-autoselect-window t)
 '(org-agenda-files (quote ("~/.org/computers.org")))
 '(put (quote upcase-region) t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(visible-bell t)
 '(x-stretch-cursor t))
;; }}}

;; {{{ Testing

;; Insert a newline when type C-n on the buffer end
(setq next-line-add-newlines t)

(require 'etags)
(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-.") 'ido-find-file-in-tag-files)

(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
	    (unless (integerp x)
	      (push (prin1-to-string x t) tag-names)))
	  tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(subword-mode t)
(require 'smart-symbol)
(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)
(load "escreen.el")
(escreen-install)

;; Fly between windows with Alt+(up|down|right|left)
(windmove-default-keybindings 'meta)

;; Deleting trailing whitespace on save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
;; }}}

(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; set foldmethode=marker

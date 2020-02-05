;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   with-editor-emacsclient-executable "emacsclient"
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(php
     nginx
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      spacemacs-default-company-backends '(company-dabbrev-code
                                                           company-gtags
                                                           company-etags
                                                           company-keywords
                                                           company-tmux))
     asm
     asciidoc
     better-defaults
     bibtex
     (c-c++ :variables
            c-c++-backend 'lsp-clangd
            c-c++-adopt-subprojects t
            c-c++-lsp-enable-semantic-highlight t)
     csv
     ;; erlang
     ;; elixir
     git
     github
     emacs-lisp
     (go :variables go-use-gometalinter t)
     haskell
     helm
     html
     games
     java
     javascript
     json
     ;; julia
     lua
     latex
     lsp
     major-modes
     markdown
     (mu4e :variables
           mu4e-enable-mode-line t
           mu4e-enable-notifications t
           mu4e-get-mail-command "mbsync -Va"
           mu4e-change-filenames-when-moving t
           mu4e-maildir (expand-file-name "~/mail")
           mu4e-sent-folder   "/thalheim.io/Sent"
           mu4e-drafts-folder "/thalheim.io/Drafts"
           mu4e-trash-folder  "/thalheim.io/Trash"
           mu4e-enable-async-operations t
           mu4e-use-maildirs-extension t
           mu4e-view-show-addresses t
           mu4e-maildirs-extension-hide-empty-maildirs t
           mu4e-update-interval 300
           mu4e-alert-interesting-mail-query (concat
                                              "flag:unread"
                                              " AND NOT flag:trashed"
                                              " AND NOT maildir:/thalheim.io/Spam"
                                              " AND NOT maildir:/thalheim.io/Trash"
                                              " AND NOT maildir:/thalheim.io/Entwickler"
                                              " AND NOT maildir:/thalheim.io/Netzwerke"
                                              " AND NOT maildir:/thalheim.io/zlist/*")
           mu4e-user-mail-address-list '("joerg@thalheim.io" "joerg@higgsboson.tk" "s1691654@sms.ed.ac.uk")
           message-send-mail-function 'smtpmail-send-it
           smtpmail-smtp-server "mail.thalheim.io"
           smtpmail-smtp-user "joerg@higgsboson.tk"
           smtpmail-smtp-service 587
           smtpmail-stream-type 'starttls
           mml-secure-openpgp-encrypt-to-self t)
     org
     ;; notmuch
     ;; ocaml
     perl5
     (nixos :variables nixos-enable-company nil)

     deft
     ;;(python :variables python-backend 'lsp)
     python
     racket
     restructuredtext
     ruby
     rust
     ranger
     ;; scala
     shell-scripts
     (spell-checking :variables enable-flyspell-auto-completion t)
     sql
     syntax-checking
     systemd
     ;; typescript
     treemacs
     vimscript
     version-control
     yaml
     zig)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(coffee-mode
                                      csharp-mode
                                      blacken
                                      direnv
                                      editorconfig
                                      flycheck-inline
                                      flycheck-pycheckers
                                      sieve
                                      meson-mode
                                      persistent-scratch
                                      ;org-caldav
                                      (bitwarden
                                       :location (recipe :fetcher github :repo "seanfarley/emacs-bitwarden"))
                                      (company-tmux
                                       :location (recipe :fetcher github :repo "Mic92/company-tmux"))
                                      (osc52
                                       :location (recipe :fetcher github :repo "Mic92/osc52"))
                                      (defaultencrypt
                                        :location (recipe :fetcher gitlab :repo "lechten/defaultencrypt")))
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '(agda2-mode)
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light solarized-dark gruvbox spacemacs-dark spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("SauceCodePro Nerd Font Mono"
                               :size 26
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;;dotspacemacs-mode-line-theme 'vim-powerline
   dotspacemacs-mode-line-theme 'spacemacs
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
)

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))
  (setq lsp-message-project-root-warning t)

  (global-company-mode)

  (use-package osc52
    :config (osc52-set-cut-function))

  (use-package jl-encrypt
    :config (progn
              (add-hook 'message-send-hook 'mml-secure-encrypt-if-possible)
              (add-hook 'message-send-hook 'mml-secure-check-encryption-p)))

  (use-package bitwarden
    :config (progn
              (bitwarden-auth-source-enable)
              (setq bitwarden-automatic-unlock "")
             ))

  (use-package persistent-scratch
    :config
    (persistent-scratch-setup-default))

  (setq vc-follow-symlinks t)

  ;(setq org-caldav-url "https://cloud.thalheim.io/remote.php/dav/calendars/joerg@higgsboson.tk"
  ;      org-caldav-calendar-id "personal"
  ;      org-caldav-inbox "~/.org/calendar.org"
  ;      org-caldav-files '("~/.org/calendar.org")
  ;      org-icalendar-timezone "Europe/London"
  ;      org-agenda-files (list "~/.org/calendar.org"))

  ;; depending on the language treat _ or - as part of a variable name
  (with-eval-after-load 'evil
    :config (defalias #'forward-evil-word #'forward-evil-symbol))

  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  (global-flycheck-inline-mode)
  (global-flycheck-mode 1)
  ;; check both mypy and flake8
  (setq flycheck-pycheckers-checkers '(flake8 mypy2 mypy3))
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)

  (setq org-latex-prefer-user-labels t)

  (with-eval-after-load 'yapfify
    (defun yapfify-buffer ()
       "Format python code with isort and black"
       (interactive)
       (py-isort-buffer)
       (blacken-buffer)))

  (use-package treemacs
    :config (progn
              (treemacs-git-mode 'deferred)
              (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)))

  ;; otherwise nix-mode will block on instantiating stuff
  (setenv "NIX_REMOTE_SYSTEMS" "")
  (use-package direnv
    :config (direnv-mode))


  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    (mu4e-alert-set-default-style 'notifications))

  (add-to-list 'mu4e-header-info-custom
               '(:folder . (:name "Folder"  ;; long name, as seen in the message-view
                            :shortname "Folder"           ;; short name, as seen in the headers view
                            :help "Mailbox folder of the message" ;; tooltip
                            :function (lambda (msg) (replace-regexp-in-string "/thalheim.io/?\\(zlist/\\)?" "" (mu4e-message-field msg :maildir))))))
  (setq mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:folder . 20) (:from . 22) (:subject)))

  (setq mu4e-spam-folder "/thalheim.io/Spam")

  ;; Mark as read and move to spam
  (add-to-list 'mu4e-marks
               '(spam
                 :char       "S"
                 :prompt     "Spam"
                 :show-target (lambda (target) mu4e-spam-folder)
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid mu4e-spam-folder "+S-u-N"))))

  (mu4e~headers-defun-mark-for spam)
  (define-key mu4e-headers-mode-map (kbd "S") 'mu4e-headers-mark-for-spam)

  (setq user-full-name  "Jörg Thalheim"
        user-mail-address "joerg@thalheim.io")
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name  "Big messages"
                :query "size:2M..500M"
                :key ?b))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (with-eval-after-load 'sh-script
    (lambda ()
      (setq sh-basic-offset 2 sh-indentation 2)))

  (add-to-list 'spacemacs-indent-sensitive-modes 'c-mode)
  (add-to-list 'spacemacs-indent-sensitive-modes 'c++-mode)

  (defun toggle-tabs-spaces ()
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode)))

  (spacemacs/set-leader-keys "tt" 'toggle-tabs-spaces)

  (setq deft-directory "~/Sync/notes")
  (setq org-directory "~/Sync/notes")

  (setq lsp-clients-clangd-args '("-background-index"))

  (setq projectile-project-search-path '("~/git"))

  (defun copy-current-line-position-to-clipboard ()
    "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
    (interactive)
    (let ((path-with-line-number
           (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
      (kill-new path-with-line-number)
      (message (concat path-with-line-number " copied to clipboard"))))

  (spacemacs/set-leader-keys "rf" 'copy-current-line-position-to-clipboard)

  ; make recentf unique per host in case .emacs.d is stored in a NFS share to avoid lock contention
  (setq recentf-save-file (expand-file-name (concat "recentf-" system-name) "/home/joerg/.emacs.d/.cache/"))

  (defun shell-stdout-to-string (command)
    (with-output-to-string
      (with-current-buffer
          standard-output
        (process-file shell-file-name nil '(t nil)  nil shell-command-switch command))))

  (defun insert-nix-hash (attribute)
    (interactive "sNix attribute to fetch")
    (let ((hash (shell-stdout-to-string (concat "nix-prefetch " attribute))))
      ;; replace current selection → can detect the hash under the cursor as well?
      (if (use-region-p)
          (kill-region (region-beginning)
                       (region-end)))
      (insert (string-trim-right hash))))

  (spacemacs/set-leader-keys "xn" 'insert-nix-hash)

  ;; speeds-up tmux/screen:
  ;; https://github.com/syl20bnr/spacemacs/issues/6181#issuecomment-264549368
  (eval-after-load "xterm" ;; term/xterm.el does not provide 'xterm
    '(defadvice xterm--query (around tweak-for-gnu-screen (query handlers) activate)
     ;; GNU screen does not support this sequence
       (unless (string= query "\e]11;?\e\\")
         ad-do-it)))
)


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zig-mode yaml-mode x86-lookup web-mode web-beautify vimrc-mode typit mmt tagedit systemd sudoku sql-indent slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv ranger rake racket-mode yapfify unfill smeargle pyvenv pytest pyenv-mode py-isort pip-requirements orgit mwim mmm-mode markdown-toc magit-gitflow live-py-mode hy-mode dash-functional helm-pydoc helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-helm flyspell-correct flycheck-pycheckers evil-magit magit magit-popup git-commit ghub treepy graphql with-editor cython-mode company-anaconda blacken auto-dictionary anaconda-mode pythonic nix-mode helm-nixos-options helm-company helm-c-yasnippet fuzzy company-statistics company-nixos-options nixos-options company auto-yasnippet yasnippet ac-ispell auto-complete toml-mode racer flycheck-rust cargo markdown-mode rust-mode flycheck-pos-tip pos-tip flycheck ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (persistent-scratch zig-mode yaml-mode x86-lookup web-mode web-beautify vimrc-mode typit mmt tagedit systemd sudoku sql-indent slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv ranger rake racket-mode yapfify unfill smeargle pyvenv pytest pyenv-mode py-isort pip-requirements orgit mwim mmm-mode markdown-toc magit-gitflow live-py-mode hy-mode dash-functional helm-pydoc helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-helm flyspell-correct flycheck-pycheckers evil-magit magit magit-popup git-commit ghub treepy graphql with-editor cython-mode company-anaconda blacken auto-dictionary anaconda-mode pythonic nix-mode helm-nixos-options helm-company helm-c-yasnippet fuzzy company-statistics company-nixos-options nixos-options company auto-yasnippet yasnippet ac-ispell auto-complete toml-mode racer flycheck-rust cargo markdown-mode rust-mode flycheck-pos-tip pos-tip flycheck ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
           (quote arglist-cont-nonempty)
           (quote
            (c-lineup-gcc-asm-reg c-lineup-arglist)))
     (eval c-set-offset
           (quote arglist-close)
           0)
     (eval c-set-offset
           (quote arglist-intro)
           (quote ++))
     (eval c-set-offset
           (quote case-label)
           0)
     (eval c-set-offset
           (quote statement-case-open)
           0)
     (eval c-set-offset
           (quote substatement-open)
           0)
     (javascript-backend . tern)
     (javascript-backend . lsp)
     (go-backend . go-mode)
     (go-backend . lsp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3")))))
)

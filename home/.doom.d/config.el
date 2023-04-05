;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jörg Thalheim"
      user-mail-address "joerg@thalheim.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq deft-directory "~/Sync/notes")
(setq org-directory "~/Sync/notes")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(after! 'sh-script
  (lambda ()
    (setq sh-basic-offset 2 sh-indentation 2)))

(use-package! lsp
  :custom
  (lsp-clients-clangd-args '("-background-index"))
  (lsp-enable-suggest-server-download nil))

(setq projectile-project-search-path '("~/git"))


(add-to-list '+format-on-save-enabled-modes 'go-mode t)

;; make recentf unique per host in case .emacs.d is stored in a NFS share to avoid lock contention
(setq recentf-save-file
      (expand-file-name (concat "recentf-" system-name) "/home/joerg/.emacs.d/.local/.cache/"))

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

(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(map! (:leader
       (:desc "search" :prefix "r"
        :desc "Nix hash" :nv "n" #'insert-nix-hash
        :desc "Line" :n "l" #'copy-current-line-position-to-clipboard)))

;; otherwise nix-mode will block on instantiating stuff
(setenv "NIX_REMOTE_SYSTEMS" "")

(setq persistent-scratch-save-file (expand-file-name "~/.emacs.d/.persistant-scratch"))

(use-package! tree-sitter
  :config
  (cl-pushnew (expand-file-name "~/.tree-sitter") tree-sitter-load-path)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! flycheck
  :config
  (setq-default flycheck-disabled-checkers '(python-pylint)))

(setq-hook! 'nix-mode-hook company-idle-delay nil)

(setq sentence-end-double-space nil) ;period single space ends sentence

(use-package envrc
    :config
    (envrc-global-mode))

(use-package! lsp-mode
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.terragrunt-cache\\'")
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nil"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 2)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

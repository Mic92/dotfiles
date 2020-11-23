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
(setq doom-theme 'doom-solarized-light)

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


(use-package! mu4e
  :config
  (setq mu4e-enable-mode-line t
        mu4e-maildirs-extension-fake-maildir-separator "\\."
        mu4e-enable-notifications t
        mu4e-change-filenames-when-moving t
        mu4e-maildir (expand-file-name "~/mail")
        mu4e-sent-folder "/thalheim.io/.Sent"
        mu4e-drafts-folder "/thalheim.io/.Drafts"
        mu4e-trash-folder "/thalheim.io/.Trash"
        mu4e-spam-folder "/thalheim.io/.Spam"
        mu4e-enable-async-operations t
        mu4e-use-maildirs-extension t
        mu4e-view-show-addresses t
        mu4e-maildirs-extension-hide-empty-maildirs t
        mu4e-update-interval 300
        mu4e-user-mail-address-list '("joerg@thalheim.io" "joerg@higgsboson.tk" "s1691654@sms.ed.ac.uk")
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "mail.thalheim.io"
        smtpmail-smtp-user "joerg@higgsboson.tk"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:folder . 20) (:from . 22) (:subject))
        mml-secure-openpgp-encrypt-to-self t
        )

  (setq nsm-settings-file (expand-file-name "~/.emacs.d/network-security.data"))

  (add-to-list 'mu4e-header-info-custom
               '(:folder . (:name "Folder"  ;; long name, as seen in the message-view
                            :shortname "Folder"           ;; short name, as seen in the headers view
                            :help "Mailbox folder of the message" ;; tooltip
                            :function (lambda (msg) (replace-regexp-in-string "/thalheim.io/?\\(.zlist\\)?" "" (mu4e-message-field msg :maildir))))))
  (add-to-list 'mu4e-headers-actions
               '("Apply patch" . mu4e-action-git-apply-mbox) t)

  ;; Mark as read and move to spam
  (add-to-list 'mu4e-marks
               '(spam
                 :char       "S"
                 :prompt     "Spam"
                 :show-target (lambda (target) mu4e-spam-folder)
                 :action      (lambda (docid msg target)
                                (mu4e~proc-move docid mu4e-spam-folder "+S-u-N"))))

  (mu4e~headers-defun-mark-for spam)
  (map! :localleader
        :map mu4e-headers-mode-map
        :desc "Move to spam" "s" #'mu4e-headers-mark-for-spam)

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name  "Big messages"
                :query "size:2M..500M"
                :key ?b))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Unread messages without spam"
                :query "flag:unread AND NOT flag:trashed AND NOT maildir:/thalheim.io/.Spam"
                :key ?u)))

(after! 'sh-script
  (lambda ()
    (setq sh-basic-offset 2 sh-indentation 2)))

(use-package! lsp
  :config
  (setq lsp-clients-clangd-args '("-background-index")))

(setq projectile-project-search-path '("~/git"))
(yas/load-directory "~/.homesick/repos/dotfiles/home/.emacs.d/snippets")

(add-to-list '+format-on-save-enabled-modes 'go-mode t)
(add-hook! 'go-mode-hook
  (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
  (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))

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

;; FIXME not loaded as a proper package yet
;;(use-package! bitwarden
;;  :config
;;  (bitwarden-auth-source-enable)
;;  (setq bitwarden-automatic-unlock (lambda() "")))
(bitwarden-auth-source-enable)
(setq bitwarden-automatic-unlock (lambda() ""))

(defun treemacs-back-and-forth ()
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (other-window 1)
    (treemacs-select-window)))

(use-package! treemacs-evil
  :config
  (define-key! evil-treemacs-state-map
    "-" #'treemacs-back-and-forth))

(use-package! treemacs
  :config
  (treemacs-follow-mode t))

(map! :leader :n "-" #'treemacs-back-and-forth)

(setq persistent-scratch-save-file (expand-file-name "~/.emacs.d/.persistant-scratch"))

;; Is there an easier way?
(defun disable-python-pylint()
  (flycheck-disable-checker 'python-pylint))

(use-package! flycheck
  :config
  (add-hook 'flycheck-before-syntax-check-hook
            #'disable-python-pylint 'local))

(use-package! direnv
  :config
  (setq direnv--executable "emacs-direnv"))

(setq-hook! 'nix-mode-hook company-idle-delay nil)

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

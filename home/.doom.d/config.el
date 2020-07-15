;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


(use-package! hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar doom--hl-line-mode nil)

  (add-hook! '(evil-visual-state-entry-hook activate-mark-hook)
    (defun doom-disable-hl-line-h ()
      (when hl-line-mode
        (setq-local doom--hl-line-mode t)
        (hl-line-mode -1))))

  (add-hook! '(evil-visual-state-exit-hook deactivate-mark-hook)
    (defun doom-enable-hl-line-maybe-h ()
      (when doom--hl-line-mode
        (hl-line-mode +1)))))

(setq mu4e-enable-mode-line t)
(setq mu4e-maildirs-extension-fake-maildir-separator "\\.")
(setq mu4e-enable-notifications t)
(setq mu4e-get-mail-command "mbsync -Va")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-maildir (expand-file-name "~/mail"))
(setq mu4e-sent-folder   "/thalheim.io/.Sent")
(setq mu4e-drafts-folder "/thalheim.io/.Drafts")
(setq mu4e-trash-folder  "/thalheim.io/.Trash")
(setq mu4e-enable-async-operations t)
(setq mu4e-use-maildirs-extension t)
(setq mu4e-view-show-addresses t)
(setq mu4e-maildirs-extension-hide-empty-maildirs t)
(setq mu4e-update-interval 300)
(setq mu4e-alert-interesting-mail-query (concat
                                    "flag:unread"
                                    " AND NOT flag:trashed"
                                    " AND NOT maildir:/thalheim.io/.Spam"
                                    " AND NOT maildir:/thalheim.io/.Trash"
                                    " AND NOT maildir:/thalheim.io/.Entwickler"
                                    " AND NOT maildir:/thalheim.io/.Netzwerke"
                                    " AND NOT maildir:/thalheim.io/.zlist.*"))
(setq mu4e-user-mail-address-list '("joerg@thalheim.io" "joerg@higgsboson.tk" "s1691654@sms.ed.ac.uk"))
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "mail.thalheim.io")
(setq smtpmail-smtp-user "joerg@higgsboson.tk")
(setq smtpmail-smtp-service 587)
(setq smtpmail-stream-type 'starttls)
(setq mml-secure-openpgp-encrypt-to-self t)

(with-eval-after-load 'mu4e-alert
                      ;; Enable Desktop notifications
  (mu4e-alert-set-default-style 'notifications))

;(add-to-list 'mu4e-header-info-custom
;             '(:folder . (:name "Folder"  ;; long name, as seen in the message-view
;                                :shortname "Folder"           ;; short name, as seen in the headers view
;                                :help "Mailbox folder of the message" ;; tooltip
;                                :function (lambda (msg) (replace-regexp-in-string "/thalheim.io/?\\(.zlist\\)?" "" (mu4e-message-field msg :maildir))))))
(setq mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:folder . 20) (:from . 22) (:subject)))

(setq mu4e-spam-folder "/thalheim.io/.Spam")

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
(define-key mu4e-headers-mode-map (kbd "S") 'mu4e-headers-mark-for-spam)


(map! :after treemacs
      :leader
      :n "-" #'treemacs-project-toggle)

(defun treemacs-project-toggle ()
  "Toggle and add the current project to treemacs if not already added."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((path (projectile-ensure-project (projectile-project-root)))
          (name (projectile-project-name)))
      (unless (treemacs-current-workspace)
        (treemacs--find-workspace))
      (treemacs-do-add-project-to-workspace path name)
      (treemacs-select-window))))

(map! :after treemacs
      :leader
      :n "+" #'treemacs-project-toggle)

(defun treemacs-back-and-forth ()
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (other-window 1)
    (treemacs-select-window)))

(use-package! treemacs-evil
  :config
  (define-key! evil-treemacs-state-map
    "-" #'treemacs-back-and-forth))

(map! :after treemacs
      :leader
      :n "-" #'treemacs-back-and-forth)

(setq user-full-name  "Jörg Thalheim"
      user-mail-address "joerg@thalheim.io")
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
               :name  "Big messages"
               :query "size:2M..500M"
               :key ?b))
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
               :name "Unread messages without spam"
               :query "flag:unread AND NOT flag:trashed AND NOT maildir:/thalheim.io/.Spam"
               :key ?u))

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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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

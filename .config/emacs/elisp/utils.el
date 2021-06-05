;; -*- lexical-binding: t; -*-

;; No-littering
;; keeps your `user-emacs-directory' clean by moving files or directories
;; created by some packages they need to be set before loading no-littering
(setq no-littering-etc-directory "~/.local/share/emacs/etc")
(setq no-littering-var-directory "~/.local/share/emacs/var")
(use-package no-littering)

;;(use-package esup)

;; Gcmh
;; dynamicaly alter the `gc-cons-treshold' depending on the user-activity
;; to ensure no freezes while working
(use-package gcmh
  :hook (window-setup . gcmh-mode)
  :config
  (setq
   ;; idle time gcmh has to wait before triggering gc
   gcmh-idle-delay 10))


;; Helpful
;; provide better help
(use-package helpful
  :commands (helpful-callable
             helpful-function
             helpful-macro
             helpful-variable
             helpful-command
             helpful-key))

(provide 'utils.el)

;; -*- lexical-binding: t; -*-

;; No-littering
;; keeps your `user-emacs-directory' clean by moving files or directories
;; created by some packages they need to be set before loading no-littering
(setq no-littering-etc-directory "~/.local/share/emacs/etc")
(setq no-littering-var-directory "~/.local/share/emacs/var")
(use-package no-littering)

(provide 'init-no-litering.el)

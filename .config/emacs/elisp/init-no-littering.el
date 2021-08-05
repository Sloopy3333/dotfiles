;; -*- lexical-binding: t; -*-

;; No-littering
;; keeps your `user-emacs-directory' clean by moving files or directories
;; created by some packages they need to be set before loading no-littering

;; set some important directories for emacs
(setq user-emacs-directory "~/.local/share/emacs")
;(setq custom-file "~/.config/emacs/custom.el")

;; keep custom.el in /tmp
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(setq no-littering-etc-directory "~/.local/share/emacs/etc")
(setq no-littering-var-directory "~/.local/share/emacs/var")
(use-package no-littering)

(provide 'init-no-litering.el)

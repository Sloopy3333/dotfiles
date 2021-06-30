;; -*- lexical-binding: t; -*-

;; Evil
(use-package evil
  :hook (emacs-startup . evil-mode)
  :init
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil))

(provide 'init-evil.el)

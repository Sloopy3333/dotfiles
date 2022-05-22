;; -*- lexical-binding: t; -*-
(use-package autoinsert
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory "~/.config/emacs/templates")
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (define-auto-insert "\\.html?$" "html-template.html"))

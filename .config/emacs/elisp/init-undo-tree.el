;; -*- lexical-binding: t; -*-

;; Undo-tree
(defvar undo-history-directory (concat user-emacs-directory "/undo-tree/")
  "Directory to save undo history files.")
(unless (file-exists-p undo-history-directory)
  (make-directory undo-history-directory t))
(setq undo-tree-history-directory-alist `(("." . ,undo-history-directory)))

(use-package undo-tree
  :after evil
  :config
  (global-undo-tree-mode)
  )

(provide 'init-undo-tree.el)

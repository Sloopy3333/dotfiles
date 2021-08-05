;; -*- lexical-binding: t; -*-

;; Magit
(setq bare-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
(setq bare-work-tree (concat "--work-tree=" (expand-file-name "~")))

(defun sam/magit-status-bare ()
  "set --git-dir and --work-tree in `magit-git-global-arguments' to `bare-git-dir' and `bare-work-tree' and calls `magit-status'"
  (interactive)
  (require 'magit-git)
  (add-to-list 'magit-git-global-arguments bare-git-dir)
  (add-to-list 'magit-git-global-arguments bare-work-tree)
  (call-interactively 'magit-status))

(defun sam/magit-status ()
  "removes --git-dir and --work-tree in `magit-git-global-arguments' and calls `magit-status'"
  (interactive)
  (require 'magit-git)
  (setq magit-git-global-arguments (remove bare-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remove bare-work-tree magit-git-global-arguments))
  (call-interactively 'magit-status))

(defun sam-magit-status (state)
  (interactive)
  (setq magit-git-executable state)
  (call-interactively 'magit-status))

(use-package magit
  :commands (:any sam/magit-status sam/magit-status-bare)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))



(provide 'init-magit.el)

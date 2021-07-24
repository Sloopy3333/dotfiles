;; -*- lexical-binding: t; -*-

(use-package doom-modeline
  :hook (emacs-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-height 15)
  (setq doom-modeline-mu4e t)
  (setq doom-modeline-workspace-name t)
  )

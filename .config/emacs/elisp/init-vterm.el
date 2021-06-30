;; -*- lexical-binding: t; -*-

;; vterm
(use-package vterm
  :commands vterm-mode
  ;;:hook (vterm-mode . hide-mode-line-mode)
  :bind (:map vterm-mode-map
              ("M-q" . nil))
  :config
  (setq vterm-buffer-name "vterm")
  (setq vterm-buffer-name-string "vterm-%s")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

(provide 'init-vterm.el)

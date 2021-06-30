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

;; a bottom terminal like vs code
(use-package window
  :straight nil
  :config
  (setq switch-to-buffer-obey-display-actions t)
  (setq frame-title-format '("%b – Emacs"))
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise nil)
  :custom
  (display-buffer-alist
   '(("\\*ansi-term\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.3)))
   ))

(provide 'shells.el)

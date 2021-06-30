;; -*- lexical-binding: t; -*-

;; Hl-line
;; enable line higlighting
(use-package hl-line
  ;; Highlights the current line
  :hook (prog-mode . global-hl-line-mode)
  :init (defvar global-hl-line-modes '(prog-mode
                                       text-mode conf-mode special-mode org-agenda-mode dired-mode))
  :config
  ;; enable relative line number mode
  (setq-default display-line-numbers 'relative)
  ;; explicitly define a width to reduce the cost of on-the-fly computation
  (setq-default display-line-numbers-width 3)
  ;; Disable number line in certiain mode
  (dolist
      (mode '(org-mode-hook
              term-mode-hook
              shell-mode-hook
              dired-mode-hook
              eshell-mode-hook
              vterm-mode-hook
              pass-mode))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(provide 'init-hl-line.el)

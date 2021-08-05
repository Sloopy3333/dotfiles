;; -*- lexical-binding: t; -*-

;; alias yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Minibuffer
(use-package minibuffer
  :straight nil
  :config
  ;; completion styles to be used by default see `completion-style-alist'
  ;; available styles are `partial-completion' `substring' `initials' `basic' `flex'
  (setq completion-styles
        '(partial-completion substring initials basic))
  ;; override default completion style of specific modes
  (setq completion-category-overrides
        '((file (styles . (flex)))
          (buffer (styles . (flex)))
          (project-file (styles . (flex)))
          (info-menu (styles . (flex)))))
  (setq completions-format 'vertical)
  (setq completion-ignore-case t)
  (setq completion-cycle-threshold t)
  (setq completion-flex-nospace nil)
  (setq completion-show-help t)
  (setq completion-pcm-complee-word-inserts-delimiters t)
  (setq read-answer-short t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq resize-mini-windows nil)
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1))

(provide 'init-minibuffer.el)

;; -*- lexical-binding: t; -*-

;; Solong mode
;; improves performance in large files by disabling some modes
(use-package so-long
  :straight nil
  :hook (prog-mode . global-so-long-mode)
  :config
  (setq so-long-threshold 400)
  ;; don't disable font-lock-mode, line-number-mode and don't make buffer read only
  (delq 'font-lock-mode so-long-minor-modes)
  (delq 'display-line-numbers-mode so-long-minor-modes)
  (delq 'buffer-read-only so-long-variable-overrides)
  ;; reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; disable save-place in large files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; disable these
  (append so-long-minor-modes
           '(eldoc-mode
             auto-composition-mode
             undo-tree-mode
             hl-fill-column-mode)))

(provide 'init-so-long.el)

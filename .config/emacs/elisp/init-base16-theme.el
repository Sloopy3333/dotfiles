;; -*- lexical-binding: t; -*-

(defun sam/toggle-themes ()
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
  (pcase current-theme
    ('base16-gruvbox-light-soft (load-theme 'base16-gruvbox-dark-soft t))
    ('base16-gruvbox-dark-soft (load-theme 'base16-gruvbox-light-soft t))
    (_ (error "base16-theme not found")))))

(use-package base16-theme
  :bind ("M-<f5>" . 'sam/toggle-themes)
  :config
  (setq base16-distinct-fringe-background nil)
  (setq base16-highlight-mode-line nil)
  (load-theme 'base16-gruvbox-light-soft t))

(provide 'init-base16-theme.el)

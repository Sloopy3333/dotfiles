;; -*- lexical-binding: t; -*-

(defun sam/toggle-themes ()
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
  (pcase current-theme
    ('solarized-light (load-theme 'solarized-dark t))
    ('solarized-dark (load-theme 'solarized-light t))
    (_ (error "base16-theme not found")))))

(use-package solarized-theme
  :bind ("M-<f5>" . 'sam/toggle-themes)
  :config
  (setq solarized-distinct-fringe-background nil)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line nil)
  (setq solarized-use-less-bold nil)
  (setq solarized-use-more-italic t)
  (setq solarized-emphasize-indicators t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-light t))

(provide 'init-solarized.el)

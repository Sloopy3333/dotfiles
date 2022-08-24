;; -*- lexical-binding: t; -*-

(defun sam/toggle-themes ()
  (interactive)
  (let ((current-theme (car custom-enabled-themes)))
  (pcase current-theme
    ('doom-solarized-light (load-theme 'doom-solarized-dark t))
    ('doom-solarized-dark (load-theme 'doom-solarized-light t))
    ('doom-gruvbox-light (load-theme 'doom-gruvbox t))
    ('doom-gruvbox (load-theme 'doom-gruvbox-light t))
    (_ (error "doom-theme not found")))))

(use-package doom-themes
  :bind ("M-<f5>" . 'sam/toggle-themes)
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t))

;; -*- lexical-binding: t; -*-

(defvar quick-switch-themes
  (let ((themes-list (list 'base16-gruvbox-dark-soft
                           'base16-gruvbox-light-soft
                           )))
    (nconc themes-list themes-list)))

(defun quick-switch-themes ()
  "cycle between dark and light theme"
  (interactive)
  (if-let* ((next-theme (cadr quick-switch-themes)))
      (progn (when-let* ((current-theme (car quick-switch-themes)))
               (disable-theme (car quick-switch-themes)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq quick-switch-themes (cdr quick-switch-themes)))

(use-package base16-theme
  :config
  (setq base16-distinct-fringe-background nil)
  (setq base16-highlight-mode-line nil)
  (load-theme 'base16-gruvbox-dark-soft t))

(provide 'init-base16-theme.el)

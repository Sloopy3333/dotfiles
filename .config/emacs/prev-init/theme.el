;; -*- lexical-binding: t; -*-

(use-package solarized-theme
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
  (load-theme 'solarized-dark-high-contrast t)
 )

;; modeline
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq battery-mode-line-format "|  %p (%t,%B) | ")
(display-battery-mode)
(setq display-time-default-load-average nil)
(setq display-time-format " %a %d %b %Y %I:%M %p |")
(setq display-time-interval 60)
(display-time-mode 1)
(setq global-mode-string '("" battery-mode-line-string display-time-string))

(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line
                         (list
                            '(:eval (format " [%s] " (+ 1 exwm-workspace-current-index)))
                            '(:eval (propertize "%b " 'face 'font-lock-constant-face))
                            "["
                            "" mode-name
                            "" vc-mode
                            "]"
                            ))
                         ;; right
                         (format-mode-line
                          (list
                            ;;"" '(:eval (shell-command-to-string "~/.config/exwm/scripts/volume"))
                            ;;"" '(:eval (shell-command-to-string "~/.config/exwm/scripts/backlight"))
                            "" mode-line-misc-info
                           ))
                         ))))


;; All-the-icons
;; adds nice icons to modline
(use-package all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon))

;; All-the-icons-dired
;; adds icons to dired
(use-package all-the-icons-dired
  ;;:after (:any dired dired-jump dired-noselect)
  :hook (dired-mode . all-the-icons-dired-mode))


;; Rainbow-delimiters
;; highlights matching brackets in same color
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'theme.el)

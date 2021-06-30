;; -*- lexical-binding: t; -*-

;; modeline
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

;;(setq battery-mode-line-format "|  %p (%t,%B) | ")
;;(display-battery-mode)
;;(setq display-time-default-load-average nil)
;;(setq display-time-format " %a %d %b %Y %I:%M %p |")
;;(setq display-time-interval 60)
;;(display-time-mode 1)
;;(setq global-mode-string '("" battery-mode-line-string display-time-string))

(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line
                         (list
                            ;;'(:eval (format " [%s] " (+ 1 exwm-workspace-current-index)))
                            "%@"
                            '(:eval (propertize "%b " 'face 'font-lock-constant-face))
                            " "
                            "("
                            "%c"
                            ":"
                            "%l"
                            ")"
                            ))
                         ;; right
                         (format-mode-line
                          (list
                            ;;"" '(:eval (shell-command-to-string "~/.config/exwm/scripts/volume"))
                            ;;"" '(:eval (shell-command-to-string "~/.config/exwm/scripts/backlight"))
 ;;                           "" mode-line-misc-info
                            "" mode-name
                            "" vc-mode
                           ))
                         ))))

(provide 'init-modeline.el)

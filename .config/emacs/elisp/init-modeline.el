;; -*- lexical-binding: t; -*-

;; modeline
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(require 'battery)
(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line
                         (list
                          (if (frame-parameter (selected-frame) 'exwm-active)
                              '(:eval (format " [%s] " (+ 1 exwm-workspace-current-index))))
                          "%@"
                          '(:eval (propertize "%b " 'face '(:inherit font-lock-type-face :weight bold)))
                          " "
                          "("
                          "%c"
                          ":"
                          "%l"
                          ")"
                          " "
                          "["
                          "" mode-name
                          "" vc-mode
                          "]"
                          ))
                        ;; right
                        (format-mode-line
                         (list
                          '(:eval
                            (propertize
                             (battery-format "%p (%b %t %r W)"
                                             (battery-linux-sysfs)) 'face '(:inherit font-lock-string-face :weight bold)))
                          " | "
                          '(:eval
                            (propertize
                             (format-time-string "%a, %b %d %I:%M %p") 'face '(:inherit font-lock-preprocessor-face :weight bold)))
                          ))
                        ))))

(provide 'init-modeline.el)

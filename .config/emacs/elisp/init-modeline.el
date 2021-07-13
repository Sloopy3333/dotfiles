;; -*- lexical-binding: t; -*-

;; modeline
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-format
              '((:eval (simple-mode-line-render
                        ;; left
                        (format-mode-line
                         (list
                          (if (frame-parameter (selected-frame) 'exwm-active)
                            '(:eval (format " [%s] " (+ 1 exwm-workspace-current-index))))
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
                            "" mode-name
                            "" vc-mode
                           ))
                         ))))

(provide 'init-modeline.el)

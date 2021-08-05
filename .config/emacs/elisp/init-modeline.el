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
                          "%@ "
                          '(:eval (propertize "%b " 'face '(:inherit font-lock-type-face :weight bold)))
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
                          "["
                          "" mode-name
                          "" vc-mode
                          "]"
                          ))
                        ))))

(provide 'init-modeline.el)

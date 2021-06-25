;; -*- lexical-binding: t; -*-

(use-package color-theme-sanityinc-solarized
  :config
  (load-theme 'sanityinc-solarized-dark t))

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
                            '(:eval (propertize "%b " 'face 'font-lock-constant-face))
                            "%I "
                            "("
                            "%C:%l"
                            ")"
                            ))
                         ;; right
                         (format-mode-line
                          (list
                            "" mode-name
                            "" vc-mode
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

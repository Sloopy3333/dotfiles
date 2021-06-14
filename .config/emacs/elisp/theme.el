;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t)
  (setq doom-themes-enable-bold t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (load-theme 'doom-solarized-dark-high-contrast t))




(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))


(setq battery-mode-line-format "%p%% (%t,%b) ")
(display-battery-mode)
(setq display-time-default-load-average 1)
(setq display-time-format "%a %d %b %Y %I:%M %p")
(setq display-time-interval 60)
(display-time-mode 1)

(setq-default mode-line-format
      (list
       '(:eval (format " [%s] " (+ 1 exwm-workspace-current-index)))
       '(:eval(list
               (when (buffer-modified-p)
                 (propertize " "
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))
               (when buffer-read-only
                 (propertize " "
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))))

       '(:eval (propertize "%m " 'face 'font-lock-keyword-face
                           'help-echo (buffer-file-name)))

       '(:eval (when-let (vc vc-mode)
                 (list " "
                       (propertize (substring vc 5)
                                   'face 'font-lock-comment-face)
                       " ")))

       (mode-line-fill 'mode-line 70)
       '(:eval (propertize (format-time-string "%a %d %b %Y %I:%M %p")
                           'help-echo
                           (concat (format-time-string "%c; ")
                                   (emacs-uptime "Uptime:%hh"))))
       " " mode-line-misc-info
       ))



;;(setq-default mode-line-format nil)
;; Doom-modeline
;; a minimal modeline from doom emacs
(use-package doom-modeline
  :disabled t
  :config
  (setq display-time-default-load-average 1)
  (display-battery-mode 1)
  (setq display-time-interval 1)
  (setq display-time-format "%a %d %b %Y %I:%M %p")
  (display-time-mode 1)
  (doom-modeline-mode 1)
  ;;(setq-default header-line-format mode-line-format)
  ;;(setq-default mode-line-format nil)
  :custom ((doom-modeline-height 15)
           (setq doom-modeline-icon nil)))
;; this is requried to display icons in modline while running emacsclient
;;(defun enable-doom-modeline-icons (_frame)
;;  (setq doom-modeline-icon t))
;;(add-hook 'after-make-frame-functions
;;          #'enable-doom-modeline-icons)
;; disable in term mode
;;(add-hook 'term-mode-hook #'hide-mode-line-mode)


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

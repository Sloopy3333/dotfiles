;; -*- lexical-binding: t; -*-

;; Fonts

;; set various face attributes
(defun sam/set-font ()
  (set-face-attribute 'default nil :inherit nil :family "IBM Plex Mono Text" :foundry "IBM" :weight 'normal :height 130)
  (set-face-attribute 'fixed-pitch nil :inherit 'default)
  (set-face-attribute 'font-lock-comment-face nil :inherit 'default :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :inherit 'default :slant 'italic))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (sam/set-font))))
  (sam/set-font))

;; moderate font-lock decoration
;;(setq font-lock-maximum-decoration nil)

;; Scroll
;; minimum distance in lines between the cursor and window before scroll starts
(setq scroll-margin 2)
;; number of lines to scroll after `scroll-margin' is reached
(setq scroll-step 1)
;; stop trying recenter cursor
(setq scroll-conservatively 999999)
;; minimum distance in columns between the cursor and window before scroll starts
(setq hscroll-margin 2)
;; number of colums to scroll after `hscroll-margin' is reached
(setq hscroll-step 1)
;; horizontally scroll only the current line
(setq auto-hscroll-mode 'current-line)
;; more performant rapid scrolling over unfontified regions.
(setq fast-but-imprecise-scrolling t)

;; disable scratch mode on startup
(setq initial-major-mode 'fundamental-mode)
;; initial buffer message
(setq initial-scratch-message nil)

;; disable startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; dont show default modeline until my modeline is loaded
(setq mode-line-format nil)

;; don't highlight non selected windows
(setq highlight-nonselected-windows nil)

;; hide cursor in non-focused window
(setq cursor-in-non-selected-window nil)

;; dialog will appear in echo area
(setq use-dialog-box nil)

;; set warning messages to emergency
(setq warning-minimum-level :emergency)

;; don't prompt when visiting new buffer file
(setq confirm-nonexistent-file-or-buffer nil)

;; use forward / for creating unique buffer name
(setq uniquify-buffer-name-style 'forward)

;; disable bell beep
(setq ring-bell-function #'ignore)

;; disable flashing
(setq visible-bell nil)

;; disable cursor blinking
(blink-cursor-mode -1)

;; disale blinking in paren matching
(setq blink-matching-paren nil)

;; don't stretch cursor
(setq x-stretch-cursor nil)

;; resize window pixelwise insted of steps
(setq frame-resize-pixelwise t)

;; resize windows inside emacs stepwise
(setq window-resize-pixelwise nil)

;; disable pop-up and `display-buffer' should make new window
(setq pop-up-windows t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'emacs-init-hook #'window-divider-mode)


;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors 'silent)



;; Print time took to load emacs in messages buffer
(defun sam/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections." (emacs-init-time) gcs-done))
(add-hook 'emacs-startup-hook #'sam/display-startup-time)

(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

(provide 'init-ui.el)

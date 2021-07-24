;; -*- lexical-binding: t; -*-

(setq default-file-name-handler file-name-handler-alist)
;; Increase gc memory and set file-name-handler-alist until init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; Reset gc memory and file-name-handler-alist after init
(add-hook 'emacs-startup-hook
          (lambda()
            (setq gc-cons-threshold (* 16 1024 1204)
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler)))
          ;;(setq read-process-output-max (* 3 1024 1024)))


;; Fix the ugly UI
(fset 'menu-bar-open nil)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;;(push '(internal-border-width . 6) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
;; disable startup screen
(setq inhibit-startup-screen t)

;; less warnings
(setq warning-minimum-level :emergency)

;; Package.el
;; disabel loading package.le
(setq package-enable-at-startup nil)
(fset #'package--ensure-init-file #'ignore)

;; set some important directories for emacs
(setq user-emacs-directory "~/.local/share/emacs")
(setq custom-file "~/.config/emacs/custom.el")

;; fonts
;; not necessary to put in early-init
;; this sets fonts before frame starts so i dont have to stare at default fonts while not running daemon
(add-to-list 'default-frame-alist
             '(font . "Hack Nerd Font Mono-11"))

(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

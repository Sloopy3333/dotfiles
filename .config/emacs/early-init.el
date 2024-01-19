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


;; user emacs dir
(setq user-emacs-directory "~/.local/share/emacs/")
;; move custom file to tmp
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
;; eln-cache
;;(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; alpha
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

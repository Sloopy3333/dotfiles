;; -*- lexical-binding: t; -*-

(defvar default-file-name-handler file-name-handler-alist)
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


;; Fix the ugly UI
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)




;; Packages
;;some settings for straight and use-package
(setq
 ;;use develop branch
 straight-repository-branch "develop"
 ;; check package modificaton whe they are edited only
 straight-check-for-modifications '(check-on-save find-when-checking)
 ;; always compile packages natively
 straight-disable-native-compile nil
 ;; disable package enable
 package-enable-at-startup nil
 ;; disable package-wuickstart
 package-quickstart nil
 autoload-compute-prefix nil
 ;; tell straight to use use-package
 straight-use-package-by-default t
 ;;if set to t  use-package will be more verbose and write packages loading information in message buffer useful when debuging startup time
 use-package-verbose nil)
(fset #'package--ensure-init-file #'ignore)
 
(setq user-emacs-directory "~/.local/share/emacs")
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

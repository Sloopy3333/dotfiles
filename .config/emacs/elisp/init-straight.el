;; -*- lexical-binding: t; -*-

;; Straight
(setq
 ;;use develop branch of straight
 straight-repository-branch "develop"
 ;; don't check packages during init
 straight-check-for-modifications nil
 ;; always compile packages natively
 straight-disable-native-compile nil
 ;; tell straight to use use-package
 straight-use-package-by-default t
 ;; install packages in use-package format
 straight-use-package-by-default t
 ;; base directories where sub-directories are created
 straight-base-dir "~/.local/share/emacs"
 ;; use diffrent build directories for diffrent version of emacs
 straight-build-dir (format "build-%s" emacs-version)
 ;; shallow clone
 straight-vc-git-default-clone-depth 1
 )

;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(provide 'init-straight.el)

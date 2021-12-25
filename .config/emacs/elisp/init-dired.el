;; -*- lexical-binding: t; -*-

;; Dired
(setq my-dired-copy-list nil)
(defun sam/dired-get-selected ()
  (interactive)
  (setq my-dired-copy-list (dired-get-marked-files)))

(defun sam/dired-paste ()
  (interactive)
  (when my-dired-copy-list
    (shell-command
     (mapconcat
      #'shell-quote-argument
      `("cp" "-r" ,@my-dired-copy-list ,".")
      " "))
    (revert-buffer :ignore-auto :noconfirm)
    (setq my-dired-copy-list nil)))

(defun sam/dired-cut ()
  (interactive)
  (when my-dired-copy-list
    (shell-command
     (mapconcat
      #'shell-quote-argument
      `("mv" ,@my-dired-copy-list ,".")
      " "))
    (revert-buffer :ignore-auto :noconfirm)
    (setq my-dired-copy-list nil)))

(defun sam/dired-find-file ()
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (dired-find-alternate-file)
    (dired-find-file)))

(defun sam/dired-rename-buffer ()
  "rename dired buffer name to unique name Dired:`dired-current-directory'"
  (let ((name (generate-new-buffer-name (concat "Dired:" (dired-current-directory)))))
    (rename-buffer name)))

(use-package dired
  :straight nil
  :config
  (add-hook 'dired-after-readin-hook 'sam/dired-rename-buffer)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lhDA --group-directories-first")
  (setq ls-lisp-dirs-first t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-ls-F-marks-symlinks t)
  (setq dired-hide-details-mode t)
  (setq dired-free-space 'first)
  :hook (dired-mode . (lambda ()
                  (dired-hide-details-mode)
                  (auto-revert-mode)
                  (evil-local-set-key 'normal (kbd "l")  'sam/dired-find-file)
                  (evil-local-set-key 'normal (kbd "h")  '(lambda () (interactive)(find-alternate-file "..")))
                  (evil-local-set-key 'normal (kbd "v")  'dired-mark)
                  (evil-local-set-key 'normal (kbd "dd") 'dired-do-delete)
                  (evil-local-set-key 'normal (kbd "u")  'dired-unmark)
                  (evil-local-set-key 'normal (kbd "U")  'dired-unmark-all-marks)
                  (evil-local-set-key 'normal (kbd "yy") 'sam/dired-get-selected)
                  (evil-local-set-key 'normal (kbd "p")  'sam/dired-paste)
                  (evil-local-set-key 'normal (kbd "xx") 'sam/dired-cut)
                  (evil-local-set-key 'normal (kbd "cw") '(lambda (x) (interactive "srename:") (rename-file (dired-get-filename) x) (revert-buffer)))
                  (evil-local-set-key 'normal (kbd "mf") '(lambda (x) (interactive "stouch:") (make-empty-file x) (revert-buffer)))
                  (evil-local-set-key 'normal (kbd "md") '(lambda (x) (interactive "smkdir:") (make-directory x) (revert-buffer)))
                  (evil-local-set-key 'normal (kbd "cp") 'dired-do-chmod)
                  (evil-local-set-key 'normal (kbd "cg") 'dired-do-chgrp)
                  (evil-local-set-key 'normal (kbd "co") 'dired-do-chown)
                  (evil-local-set-key 'normal (kbd "ex") 'dired-do-compress)
                  (evil-local-set-key 'normal (kbd "W")  'wdired-change-to-wdired-mode)
                  (evil-local-set-key 'normal (kbd "w")  'eshell)
                  (evil-local-set-key 'normal (kbd "r")  'revert-buffer)
                  (evil-local-set-key 'normal (kbd "gh") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/"))))
                  (evil-local-set-key 'normal (kbd "gc") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/.config"))))
                  (evil-local-set-key 'normal (kbd "gd") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/downloads"))))
                  (evil-local-set-key 'normal (kbd "gp") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/projects"))))
                  ;;(evil-local-set-key 'normal (kbd "gs") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/scripts"))))
                  (evil-local-set-key 'normal (kbd "ge") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/external"))))
                  (evil-local-set-key 'normal (kbd "Q")  'kill-this-buffer)
                  )))

(use-package openwith
  :config
  (setq openwith-associations
        (cond
         ((string-equal system-type "darwin")
          '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
             "open" (file))
            ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "open" ("-a" "VLC" file))))
         ((string-equal system-type "gnu/linux")
          '(("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "mpv" (file))))))
  (openwith-mode +1))

(provide 'init-dired.el)


(defun sam/eshell-clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/clear (&optional scrollback)
  "Scroll contents of eshell window out of sight, leaving a blank window.
If SCROLLBACK is non-nil, clear the scrollback contents."
  (interactive)
  (if scrollback
      (eshell/clear-scrollback)
    (insert (make-string (window-size) ?\n))
    (eshell-send-input)))

(defun sam/run-command-in-eshell (cmd)
  "Runs the command 'cmd' in eshell."
  (with-current-buffer "*eshell*"
    (end-of-buffer)
    (eshell-kill-input)
    (message (concat "Running in Eshell: " cmd))
    (insert cmd)
    (eshell-send-input)
    (end-of-buffer)
    (eshell-bol)
    (yank)))

(defun sam/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun sam/eshell-prompt ()
  ;;(require 'magit)
  (let ((current-branch (magit-get-current-branch)))
    (concat
     "\n"
     (propertize (concat user-real-login-name "@" system-name) 'face `(:inherit font-lock-keyword-face))
     " "
     (propertize (sam/get-prompt-path) 'face `(:inherit font-lock-type-face :weight bold))
     (when current-branch
       (concat
        " "
        (propertize (concat " " current-branch) 'face `(:inherit font-lock-string-face))))
     " "
     (if (= (user-uid) 0)
         (propertize "" 'face `(:inherit error))
       (propertize "" 'face `(:inherit font-lock-string-face)))
     " ")))

(use-package eshell
  :config
  ;;(eshell-hist-initialize)
   ;; truncate buffer for performance
  ;;(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-directory-name "~/.config/emacs/eshell"
        eshell-aliases-file (expand-file-name "~/.config/emacs/eshell/alias"))

  (setq eshell-prompt-function      'sam/eshell-prompt
        eshell-prompt-regexp        "^.*"
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))


(provide 'init-eshell.el)

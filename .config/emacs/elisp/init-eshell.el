
(defun sam/eshell-clear-buffer ()
  "clear the current eshell buffer"
  (interactive)
   (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))

(defun sam/eshell-sudo-open (filename)
  "Open a file as root in Eshell."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

(defun sam/eshell-run-cmd (cmd)
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
  (require 'magit)
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
     (if (= eshell-last-command-status 1)
         (propertize "" 'face `(:foreground "#FF6666"))
       (propertize "" 'face `(:foreground "#A6E22E")))
     " ")))


(defun sam/eshell-rename-buffer ()
  "rename eshell buffer name to unique name Eshell:`default-directory'"
  (let (($buf (generate-new-buffer-name (concat "Eshell:" default-directory))))
    (rename-buffer $buf)))

(defun sam/eshell-bind-key ()
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-l") 'sam/eshell-clear-buffer)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-J") 'eshell-next-prompt)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-K") 'eshell-previous-prompt)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-k") 'eshell-previous-input)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-j") 'eshell-next-input)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<up>") 'eshell-previous-input)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<down>") 'eshell-next-input)
  (evil-normalize-keymaps)
  )

(use-package eshell
  :commands eshell
  :config
  ;; default eshell directory
  (add-hook 'eshell-mode-hook #'sam/eshell-rename-buffer)
  (add-hook 'eshell-directory-change-hook #'sam/eshell-rename-buffer)
  (add-hook 'eshell-mode-hook #'sam/eshell-clear-buffer)
  (setq eshell-prompt-function 'sam/eshell-prompt)
  (setq eshell-prompt-regexp "^.*")
  (setq eshell-history-size 10000)
  (setq eshell-buffer-maximum-lines 10000)
  (setq eshell-hist-ignoredups t)
  (setq eshell-highlight-prompt t)
  (setq eshell-scroll-to-bottom-on-input t)
  (setq eshell-prefer-lisp-functions nil)
  (setq eshell-banner-message "")
  (sam/eshell-bind-key)
  ;; alias
  (defalias 'cl 'sam/eshell-clean)
  (defalias 'ed 'find-file-other-window)
  (defalias 'se 'sam/eshell-sudo-open))



(provide 'init-eshell.el)

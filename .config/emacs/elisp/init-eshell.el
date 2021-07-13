
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

;;(bind-keys*
;;    ("C-l" . (lambda () ; clear shell
;;                     (interactive)
;;                     (sam/run-command-in-eshell "clear 1"))))

(use-package eshell
  :config
  (local-set-key (kbd "C-<backspace>") (lambda () (interactive) (eshell/clear 1))))

(provide 'init-eshell.el)

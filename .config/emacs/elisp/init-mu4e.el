;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Mu4E
(use-package mu4e
  :defer 5
  :config
  (setq
   mu4e-completing-read-function 'completing-read                        ;; use buffer minibuffer
   mu4e-split-view 'vertical                                             ;; use vertical split
   message-kill-buffer-on-exit t                                         ;; kill message after exit
   mu4e-context-policy 'pick-first                                       ;; always pick first context
   mu4e-confirm-quit nil                                                 ;; don't ask to quit
   mu4e-change-filenames-when-moving t                                   ;; this is needed if using mbsync
   mu4e-update-interval (* 10  60)                                       ;; update in seconds
   mu4e-get-mail-command "mbsync -c /home/sam/.config/isync/mbsyncrc -a" ;; update command
   mu4e-root-maildir "~/.local/share/mail"                               ;; maildir location mu4e-compose-format-flowed t
   message-send-mail-function 'smtpmail-send-it                          ;; use smtpmail-send-it to send mail
   mu4e-view-prefer-html t                                               ;; whether to base the body display on the html-version.
                                        ;mu4e-view-use-gnus 't                                                 ;; use gnu article mode for view
   mu4e-view-show-images 't)                                             ;; show images in email
  (add-to-list 'mu4e-view-actions
               '("xViewXWidget" . my-mu4e-action-view-with-xwidget) t)

  (setq mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject)))

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))
  ;; Use fancy icons
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark '("D" . "")
        mu4e-headers-flagged-mark '("F" . "")
        mu4e-headers-new-mark '("N" . "")
        mu4e-headers-passed-mark '("P" . "")
        mu4e-headers-replied-mark '("R" . "")
        mu4e-headers-seen-mark '("S" . "")
        mu4e-headers-trashed-mark '("T" . "")
        mu4e-headers-attach-mark '("a" . "")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark '("s" . "")
        mu4e-headers-unread-mark '("u" . ""))  )

;; Wrap text in messages
;;(add-hook 'mu4e-view-mode-hook truncate-lines nil)

;; Html mails might be better rendered in a browser
;;(add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser)))

;;(require 'org-mu4e)
;;(setq org-capture-templates
;;  `(("m" "Email Workflow")
;;    ("mf" "Follow" entry (file+olp "~/org/mail.org" "Follow")
;;          "* TODO %:formname %:subject \n %a \n %i")
;;    ("mr" "Read later" entry (file+olp "~/org/mail.org" "Read")
;;          "* TODO %a \n\n  %i")))



(provide 'init-mu4e.el)

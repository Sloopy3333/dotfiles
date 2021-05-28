;; -*- lexical-binding: t; -*-

(use-package exwm
  :init
  (setq exwm-workspace-index-map (lambda(index) (number-to-string (1+ index))))
  (setq exwm-workspace-number 5)
  :config
;; Set the initial number of workspaces (they can also be created later).
;; rename exwm windows
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
  '(?\C-x
    ?\C-u
    ?\C-h
    ?\M-x
    ?\M-`
    ?\M-&
    ?\M-:
    ?\M-:
    ?\M-j
    ?\\ -h
    ?\S-\ ))

;; any key followed by C-q will be sent to the focused window rather than exwm
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; Global keybindings
(setq exwm-input-global-keys
      `(
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; switch workspace
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))

        ([?\s-r] . exwm-reset)
        ([?\s-&] . (lambda (command)
 		     (interactive (list (read-shell-command "$ ")))
 		     (start-process-shell-command command nil command)))

        ([?\s-q] . (lambda() (interactive) (kill-this-buffer) (delete-window)))
        ([?\s-j] . windmove-down)
        ([?\s-k] . windmove-up)
        ([?\s-l] . windmove-right)
        ([?\s-h] . windmove-left)

        ([?\s-s] . split-window-right)
        ([?\s-v] . split-window-below)

        ([?\s-J] . windmove-swap-states-down)
        ([?\s-K] . windmove-swap-states-up)
        ([?\s-L] . windmove-swap-states-right)
        ([?\s-H] . windmove-swap-states-left)

	([?\s-b] . (lambda() (interactive) (start-file-process-shell-command "brave" "brave" "brave")))
        ;;([?\s-C-t] . exwm-floating-toggle-floating)
        ;;([?\s-C-f] . exwm-layout-toggle-fullscreen)
))

;; hide the minibuffer and echo area when they're not used, by
(setq exwm-workspace-minibuffer-position 'bottom)

;; system tray
;;(require 'exwm-systemtray)
;;(exwm-systemtray-enable)
(exwm-enable))

;; -*- lexical-binding: t; -*-

;; dmenu_run replacement
(let  ((system-executables (seq-mapcat
                            (lambda (bin)
                              (directory-files bin nil "^[^.]"))
                            exec-path)))
  (defun sam/exwm-dmenu-run (command)
    (interactive (list (completing-read "Execute: " system-executables)))
    (start-process-shell-command command nil command)))


(defun sam/exwm-kill-buffer-or-window ()
  "kill the buffer or window"
  (interactive)
  (if(> (count-windows) 1)
      (delete-window)
    (kill-this-buffer)))

(defun sam/exwm-switch-to-next-buffer ()
  "similar to `switch-to-next-buffer' but ignores special buffers"
  (interactive)
  (next-buffer)
  (while (string-match-p "^\\*" (buffer-name))
    (next-buffer)))

(defun sam/exwm-switch-to-prev-buffer ()
  "similar to `switch-to-prev-buffer' but ignores special buffers"
  (interactive)
  (previous-buffer)
  (while (string-match-p "^\*" (buffer-name))
    (previous-buffer)))

;; window rules
(defun sam/exwm-window-rules ()
  "window rules for exwm"
  (interactive)
  (pcase exwm-class-name
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))
    ("Pcmanfm" (exwm-layout-toggle-mode-line))
    ))

(use-package exwm
  :config
  (add-hook 'exwm-update-class-hook
            (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                (exwm-workspace-rename-buffer exwm-title)))))

  (add-hook 'exwm-manage-finish-hook 'sam/exwm-window-rules)

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

  ;; initial number of workspaces
  (setq exwm-workspace-number 5)

  ;; name workspace from 1 insted of 0
  (setq exwm-workspace-index-map
      (lambda (index) (number-to-string (1+ index))))

  ;; switch to worspace with s-i
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create (1- ,i)))))

  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-C-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-move-window (1- ,i)))))

  ;; Global keybindings
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)

          ([?\s-R] . exwm-reset)
          ([?\s-c] . exwm-restart)
          ([?\s-C] . kill-emacs)

          ([?\s-\t] . exwm-switch-to-next-buffer)
          ([?\s-a] . consult-buffer)
          ([?\s-q] . sam/exwm-kill-buffer-or-window)

          ([?\s-s] . split-window-right)
          ([?\s-v] . split-window-below)

          ([?\s-j] . windmove-down)
          ([?\s-k] . windmove-up)
          ([?\s-l] . windmove-right)
          ([?\s-h] . windmove-left)

          ([?\s-J] . windmove-swap-states-down)
          ([?\s-K] . windmove-swap-states-up)
          ([?\s-L] . windmove-swap-states-right)
          ([?\s-H] . windmove-swap-states-left)

          (, (kbd "s-C-j") . enlarge-window)
          (, (kbd "s-C-k") . shrink-window)
          (, (kbd "s-C-l") . enlarge-window-horizontally)
          (, (kbd "s-C-h") . shrink-window-horizontally)

          (, (kbd "s-<tab>") . sam/switch-to-next-buffer)
          (, (kbd "s-<iso-lefttab>") . sam/switch-to-prev-buffer)

          (, (kbd "s-C-t") . exwm-floating-toggle-floating)
          (, (kbd "s-C-f") . exwm-layout-toggle-fullscreen)

	  ([?\s-\ ] . sam/exwm-dmenu-run)
	  ([?\s-d] . sam/exwm-transient)

	  ([?\s-T] . vterm)
	  ([?\s-m] . mu4e)
	  ([?\s-f] . dired-jump)
	  ([?\s-r] . elfeed)
	  ([?\s-t] . (lambda() (interactive) (start-process-shell-command "alacritty" nil "alacritty")))
	  ([?\s-b] . (lambda() (interactive) (start-process-shell-command "librewolf" nil "librewolf")))
	  ([?\s-F] . (lambda() (interactive) (start-process-shell-command "pcmanfm" nil "pcmanfm")))
          ))

  ;; hide the minibuffer and echo area when they're not used, by
  ;; (setq exwm-workspace-minibuffer-position 'top)

  ;; system tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)


  (exwm-enable))

;; Transient
(defun sam/exwm-transient-suspend ()
  (interactive)
  (shell-command "systemctl suspend")
  )

(defun sam/exwm-transient-poweroff ()
  (interactive)
  (shell-command "systemctl poweroff")
  )

(defun sam/exwm-transient-reboot ()
  (interactive)
  (shell-command "systemctl reboot")
  )

(defun sam/exwm-transient-lock ()
  (interactive)
  (shell-command "slock")
  )

;; Pacman
(defun sam/exwm-pacman-install (command)
  (interactive (list(completing-read "Install: " (split-string(shell-command-to-string "pacman -Slq")))))
  (async-shell-command (concat "sudo pacman -S " command)))

(defun sam/exwm-pacman-uninstall (command)
  (interactive (list(completing-read "Uninstall: " (split-string(shell-command-to-string "pacman -Q")))))
  (async-shell-command (concat "sudo pacman -Rns " command)))


(defun sam/exwm-pacman-update ()
  (interactive)
  (async-shell-command "sudo pacman -Syu"))

;; Fan
(defun sam/exwm-transient-fan-start ()
  (interactive)
  (async-shell-command "sudo systemctl start --now nbfc.service"))

(defun sam/exwm-transient-fan-stop ()
  (interactive)
  (async-shell-command "sudo systemctl disable --now nbfc.service")
  )

(defun sam/exwm-transient-fan-max ()
  (interactive)
  (shell-command "nbfc set -f 0 -s 100 && nbfc set -f 1 -s 100")
  )

(defun sam/exwm-transient-fan-auto ()
  (interactive)
  (shell-command "nbfc set -f 0 -a && nbfc set -f 1 -a")
  )

;; Shell
(defun sam/exwm-transient-shell(command)
  (interactive (list(transient-args 'sam/exwm-transient)))
  (shell-command (pop command))
  )

(defun sam/exwm-transient-root-shell(command)
  (interactive (list(transient-args 'sam/exwm-transient)))
  (async-shell-command (pop command))
  )

(require 'transient)
(define-transient-command sam/exwm-transient ()
  ["System options"
   ["power options"
    ("ss" "Suspend" sam/exwm-transient-suspend)
    ("sp" "Poweroff" sam/exwm-transient-poweroff)
    ("sr" "Reboot" sam/exwm-transient-reboot)
    ("sl" "Lock" sam/exwm-transient-lock)
    ]
   ["Password Store"
    ("pc" "Copy Field" password-store-copy-field)
    ("pp" "Copy Pass" password-store-copy)
    ("pe" "Manage pass" pass)
    ]
   ["Fan Control"
    ("fa" "Fan Auto" sam/exwm-transient-fan-auto)
    ("fm" "Fan Max" sam/exwm-transient-fan-max)
    ("fe" "Start" sam/exwm-transient-fan-start)
    ("fd" "Stop" sam/exwm-transient-fan-stop)
    ]
   ["Pacman"
    ("Pu" "Uninstall" sam/exwm-pacman-update)
    ("Pi" "Install" sam/exwm-pacman-install)
    ("Pr" "Uninstall" sam/exwm-pacman-uninstall)
    ]
   ["Action"
    ("d" "Run as user" sam/exwm-transient-shell)
    ("D" "Run as Root" sam/exwm-transient-root-shell)
    ]
   ]
  )

(use-package transient
  :bind (:map transient-map
              ("<escape>" . transient-quit-one))
  )

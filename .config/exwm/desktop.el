;; -*- lexical-binding: t; -*-

;; dmenu_run replacement
(let  ((system-executables (seq-mapcat
                            (lambda (bin)
                              (directory-files bin nil "^[^.]"))
                            exec-path)))
  (defun sam/exwm-dmenu-run (command)
    "list executables in path listed in `exec-path'"
    (interactive (list (completing-read "Execute: " system-executables)))
    (start-process-shell-command command nil command)))


;; not usefull anymore
(defun sam/exwm-kill-buffer-or-window ()
  "kill the buffer or window"
  (interactive)
  (if(> (count-windows) 1)
      (kill-buffer-and-window)
    (kill-this-buffer)
    ))

;; not usefull anymore
(defun sam/exwm-bury-buffer-kill-window ()
  "kill the buffer or window"
        (interactive)
  (if(> (count-windows) 1)
      (progn(bury-buffer)
      (delete-window))
    (bury-buffer)
    ))

(defun sam/exwm-split-window-right ()
  "split window right switch to window and rename create new buffer named untitled$"
  (interactive)
  (progn
    (split-window-right)
    (windmove-right)
    (let (($buf (generate-new-buffer "untitled")))
      (switch-to-buffer $buf))))


(defun sam/exwm-split-window-below ()
  "split window below switch to window and rename create new buffer named untitled$"
  (interactive)
  (progn
    (split-window-below)
    (windmove-down)
    (let (($buf (generate-new-buffer "untitled")))
      (switch-to-buffer $buf))))

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

(defun sam/exwm-switch-next-buffer-or-window ()
  "move focus to next window if exist or cycle through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window 1)
    (sam/switch-to-next-buffer)
    ))

(defun sam/exwm-switch-prev-buffer-or-window ()
  "move focus to next window if exist or cycle through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window -1)
    (sam/switch-to-prev-buffer)
    ))

;; voulme
(defun sam/exwm-audio-raise ()
  (interactive)
  (progn
  (start-process "volume" nil "amixer" "set" "Master" "5%+")
  (force-mode-line-update)))

(defun sam/exwm-audio-down ()
  (interactive)
  (progn
(start-process "volume" nil "amixer" "set" "Master" "5%-")
  (force-mode-line-update)))

(defun sam/exwm-audio-toggle ()
  (interactive)
  (progn
(start-process "volume" nil "amixer" "set" "Master" "toggle")
  (force-mode-line-update)))

;; backlight
(defun sam/exwm-backlight-raise ()
  (interactive)
  (progn
  (start-process "backlight" nil "xbacklight" "-inc" "+2")
  (force-mode-line-update)))

(defun sam/exwm-backlight-down ()
  (interactive)
  (progn
  (start-process "backlight" nil "xbacklight" "-inc" "-2")
  (force-mode-line-update)))


(defun sam/exwm-screenshot ()
  (interactive)
  (start-process "screenshot" "a" "maim" "/home/sam/external/hdd2/screenshots/$(date +%s).png"))

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

    ;; this little bit will make sure that XF86 keys work in exwm buffers as well
    (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86AudioMute
               XF86MonBrightnessUp
               XF86MonBrightnessDown
               XF86PowerOff
               XF86AudioMute
               XF86AudioPlay
               XF86AudioStop
               XF86AudioPrev
               XF86AudioNext
               XF86ScreenSaver
               XF68Back
               XF86Forward
               Scroll_Lock
               print))
    (cl-pushnew k exwm-input-prefix-keys))

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
  (setq exwm-workspace-number 9)

  ;; name workspace from 1 insted of 0
  (setq exwm-workspace-index-map
        (lambda (index) (number-to-string (1+ index))))

 ; switch to worspace with s-i
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

         (, (kbd "s-@") . (lambda () (interactive) (exwm-workspace-move-window 2)))

          ([?\s-R] . exwm-reset)
          ([?\s-c] . exwm-restart)
          ([?\s-C] . kill-emacs)

          ([?\s-\t] . exwm-switch-to-next-buffer)
          ([?\s-a] . switch-to-buffer)
          ([?\s-q] . kill-buffer-and-window)
          ([?\s-Q] . bury-buffer)

          ([?\s-s] . sam/exwm-split-window-right)
          ([?\s-v] . sam/exwm-split-window-below)

          ([?\s-j] . sam/exwm-switch-next-buffer-or-window)
          ([?\s-k] . sam/exwm-switch-prev-buffer-or-window)
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

	  ([?\s-t] . eshell)
	  ([?\s-m] . mu4e)
	  ([?\s-f] . dired-jump)
	  ([?\s-r] . elfeed)
	  ([?\s-T] . (lambda() (interactive) (start-process-shell-command "st" nil "st")))
	  ([?\s-b] . (lambda() (interactive) (start-process-shell-command "librewolf" nil "librewolf")))
	  ([?\s-F] . (lambda() (interactive) (start-process-shell-command "pcmanfm" nil "pcmanfm")))

         (, (kbd "<XF86AudioRaiseVolume>") . sam/exwm-audio-raise)
         (, (kbd "<XF86AudioLowerVolume>") . sam/exwm-audio-down)
         (, (kbd "<XF86AudioMute>") . sam/exwm-audio-toggle)

         (, (kbd "<XF86MonBrightnessUp>") . sam/exwm-backlight-raise)
         (, (kbd "<XF86MonBrightnessDown>") . sam/exwm-backlight-down)

         (, (kbd "<print>") . sam/exwm-screenshot)

          ))

  ;; hide the minibuffer and echo area when they're not used, by
   ;;(setq exwm-workspace-minibuffer-position 'top)

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
   ]
  )

(use-package transient
  :bind (:map transient-map
              ("<escape>" . transient-quit-one))
  )

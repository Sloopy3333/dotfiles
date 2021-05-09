;;(defun sam/set-fonts (frame)
;;    (if (display-graphic-p frame)
;;        (progn
;;            (add-to-list 'default-frame-alist '(font . "Hack Nerd Font-16"))
;;            (set-face-attribute 'default nil :font "Hack Nerd Font-16")
;;            (set-face-attribute 'default nil :height 160)
;;            (set-frame-font "Hack Nerd Font-16" nil t))))
;;
;;(mapc 'sam/set-font (frame-list))
;;(add-hook 'after-make-frame-functions 'sam/set-font)
(custom-set-faces
 '(default ((t (:inherit nil :height 120 :family "Hack Nerd Font Mono")))))

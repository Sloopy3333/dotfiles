;; -*- lexical-binding:; -*-

(add-to-list 'load-path "~/.config/emacs/elisp")

;; package management
(load-library "init-straight")
(load-library "init-use-package")

;; utils
(load-library "init-no-littering")
(load-library "init-gcmh")
(load-library "init-helpful")

;; ui and themes
(load-library "init-ui")
(load-library "init-solarized")
(load-library "init-modeline")
(load-library "init-all-the-icons")

;; window manger
;;(load-library "init-exwm.el")

;; editor
(load-library "init-editor")
(load-library "init-electric")
(load-library "init-so-long")
(load-library "init-rainbow-delimiters")
(load-library "init-hl-line")
(load-library "init-paren")
(load-library "init-undo-tree")
(load-library "init-recentf")
(load-library "init-savehist")

;; minibuffer
(load-library "init-minibuffer")
(load-library "init-icomplete")
(load-library "init-vertico")
(load-library "init-consult")
(load-library "init-marginalia")

;; key bindigs
(load-library "init-evil")
(load-library "init-evil-collection")
(load-library "init-which-key")
(load-library "init-general")
;;
;;;; file mangement
(load-library "init-dired")

;; completions
(load-library "init-company")
(load-library "init-company-box")
(load-library "init-eglot")
(load-library "init-elec-pair")

;; version control
(load-library "init-magit")

;; languages
(load-library "init-haskell-mode")

;; shells
(load-library "init-ansi-term")
;;(load-library "init-vterm")


;; chat
(load-library "init-erc")

;; media
(load-library "init-emms")


;; mail
(load-library "init-mu4e")

;; org
(load-library "init-org-mode")

;; rss
(load-library "init-elfeed")

;; password
(load-library "init-pass")

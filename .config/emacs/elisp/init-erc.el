;; -*- lexical-binding: t; -*-

(use-package erc
  :straight nil
  :commands (erc-tls)
  :config
  (setq erc-server "irc.libera.chat")
  (setq erc-nick "sloopy")
  (setq erc-autojoin-channels-alist '(("irc.libera.chat" "#archlinux" "#emacs")))
  (setq erc-kill-buffer-on-part t)
  (setq erc-ato-query 'bury)
  )

(provide 'chat.el)

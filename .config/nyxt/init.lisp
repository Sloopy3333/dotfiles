(in-package :nyxt)

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
    "M-j" 'switch-buffer-next
    "M-k" 'switch-buffer-previous
    "M-b" 'switch-buffer
    "M-n" 'make-buffer
    "C-u" 'nyxt/web-mode:scroll-page-up
    "C-d" 'nyxt/web-mode:scroll-page-down
    )

;; Enable vim bindings
(define-configuration (buffer web-buffer)
    ((default-modes (append '(auto-mode nyxt::vi-normal-mode *my-keymap*) %slot-default%))))

(define-configuration (buffer prompt-buffer)
    ((default-modes (append '(nyxt::vi-insert-mode *my-keymap*) %slot-default%))))

;; Search engines
(define-configuration buffer
    ((search-engines
      (list
       (make-instance 'search-engine
                      :shortcut "aw"
                      :search-url "https://wiki.archlinux.org/index.php?search=~a"
                      :fallback-url "https://wiki.archlinux.org/")
       (make-instance 'search-engine
                      :shortcut "au"
                      :search-url "https://aur.archlinux.org/packages/?O=0&K=~a"
                      :fallback-url "https://aur.archlinux.org/")
       (make-instance 'search-engine
                      :shortcut "ap"
                      :search-url "https://archlinux.org/packages/?sort=&q=~a"
                      :fallback-url "https://archlinux.org/packages/")
       (make-instance 'search-engine
                      :shortcut "gh"
                      :search-url "https://github.com/search?q=~a"
                      :fallback-url "https://github.com/trending")
       (make-instance 'search-engine
                      :shortcut "gl"
                      :search-url "https://gitlab.com/search?search=~a"
                      :fallback-url "https://gitlab.com")
       (make-instance 'search-engine
                      :shortcut "ho"
                      :search-url "https://hoogle.haskell.org/?hoogle=~a"
                      :fallback-url "https://hoogle.haskell.org/")
       (make-instance 'search-engine
                      :shortcut "yt"
                      :search-url "https://youtube.com/results?search_query=~a"
                      :fallback-url "https://youtube.com/")
       (make-instance 'search-engine
                      :shortcut "so"
                      :search-url "https://stackoverflow.com/search?q=~a"
                      :fallback-url "https://stackoverflow.com/")
       (make-instance 'search-engine
                      :shortcut "wp"
                      :search-url "https://www.wikipedia.org/w/index.php?title=Special:Search&search=~a"
                      :fallback-url "https://www.wikipedia.org/")
       (make-instance 'search-engine
                      :shortcut "dd"
                      :search-url "https://duckduckgo.com/?kae=d&q=~a"
                      :fallback-url "https://duckduckgo.com/")
       (make-instance 'search-engine
                      :shortcut "go"
                      :search-url "https://www.google.com/search?q=~a"
                      :fallback-url "https://www.google.com")))))

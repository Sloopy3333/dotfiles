(use-package webjump
  :config
  (setq webjump-sites
        (append '(
                  (#("Google" ) .
                   [simple-query "google.com"
                                 "google.com/search?q=" ""])
                  (#("Amazon" ) .
                   [simple-query "www.amazon.com/s?k="
                                 "https://www.amazon.com/s?k=" ""])
                  (#("Youtube" ) .
                   [simple-query "www.youtube.com"
                                 "https://www.youtube.com/results?search_query=" ""])
                  (#("Reddit" ) .
                   [simple-query "www.reddit.com"
                                 "https://www.reddit.com/search/?q=" ""])
                  (#("Arch Wiki" ) .
                   [simple-query "wiki.archlinux.org"
                                 "https://wiki.archlinux.org/index.php?search=" ""])
                  (#("Arch package" ) .
                   [simple-query "archlinux.org/packages"
                                 "https://archlinux.org/packages/?sort=&q=" ""])
                  (#("AUR" ) .
                   [simple-query "aur.archlinux.org/packages"
                                 "https://aur.archlinux.org/packages/?O=0&K=" ""])
                  (#("Github" ) .
                   [simple-query "github.com"
                                 "https://github.com/search?q=" ""])
                  (#("GitLab" ) .
                   [simple-query "gitlab.com"
                                 "https://gitlab.com/search?search=" ""])
                  (#("Google Images" ) .
                   [simple-query "www.google.com/search?hl=en&tbm=isch&q="
                                 "https://www.google.com/search?hl=en&tbm=isch&q=" ""])
                  (#("Hoogle" ) .
                   [simple-query "https://hoogle.haskell.org"
                                 "https://hoogle.haskell.org/?hoogle=" ""])
                  (#("Stackoverflow" ) .
                   [simple-query "https://stackoverflow.com"
                                 "https://stackoverflow.com/search?q=" ""])
                  (#("Wayback Machine" ) .
                   [simple-query "https://web.archive.org"
                                 "https://web.archive.org/web/*/" ""])

                webjump-sample-sites))))

(provide 'init-webjump.el)

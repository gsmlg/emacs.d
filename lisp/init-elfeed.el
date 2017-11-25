(require-package 'elfeed)
(require-package 'elfeed-goodies)

(elfeed-goodies/setup)

(setq elfeed-feeds
      '(("http://blog.atom.io/feed.xml" blog editor)
        ("http://www.ruanyifeng.com/blog/atom.xml" blog technology)))

(provide 'init-elfeed)

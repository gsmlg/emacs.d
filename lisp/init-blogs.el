;;--------------------------------------------------------------------
;; Set org2blogs to publish blogs
;;--------------------------------------------------------------------

(require-package 'xml-rpc)
(require-package 'metaweblog)
(require-package 'org2blog)

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("my-blog"
         :url "http://blog.gsmlg.com/xmlrpc.php"
         :username "gsmlg")))




(provide 'init-blogs)

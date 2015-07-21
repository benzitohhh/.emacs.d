;; Set default emacs C source directory
(setq source-directory "/usr/local/emacs/emacs-24.3")

;; Git support
(add-to-list 'load-path "/usr/local/git/contrib/emacs")

;; full-ack
(setq ack-executable "/Users/immanuel_ben/bin/ack")

;; Racket (scheme)
(setq scheme-program-name "/Applications/Racketv5.3.3/bin/mzscheme")

;; python executable (comment this out to use the default)
(setq python-shell-interpreter "/usr/local/bin/ipython")

;; default directory
(setq default-directory "~/Desktop/")

;; Things to know doc
(setq things-to-know-file "~/Documents/thingsToKnow/thingsToKnowOhOh.txt")

;; tag tables
(setq tags-table-list
           '("/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/django/TAGS"))

;; visit tags
;(visit-tags-table "/Volumes/PwC-VM/TAGS")

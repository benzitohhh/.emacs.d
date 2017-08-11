;; Set default emacs C source directory
(setq source-directory "/Applications/emacs-25.1-src")

;; full-ack
(setq ack-executable "/Users/immanuel_ben/bin/ack")

;; python executable (comment this out to use the default)
(setq python-shell-interpreter "/usr/local/bin/ipython")
;(setq python-shell-interpreter "/usr/local/bin/python")

(setq conda-anaconda-home "/Users/immanuel_ben/miniconda3")

(setq jedi:server-args
      '("--sys-path" "/Users/immanuel_ben/miniconda3/lib/python3.6/site-packages"))

;; default directory
(setq default-directory "~/Desktop/")

;; Things to know doc
(setq things-to-know-file "~/Documents/thingsToKnow/thingsToKnowOhOh.txt")

;; tag tables
;(setq tags-table-list '("/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/django/TAGS"))

;; visit tags
;(visit-tags-table "/Volumes/PwC-VM/TAGS")

;; Set default emacs C source directory
(setq source-directory "/Applications/emacs-24.5-src")

;; Add /usr/local/bin to the path (otherwise emacs can't find stuff installed there)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Git support
(add-to-list 'load-path "/usr/local/git/contrib/emacs")

;; full-ack
(setq ack-executable "~/bin/ack")

;; default directory
(setq default-directory "~/Desktop/")

;; Things to know doc
;; (setq things-to-know-file "~/Documents/thingsToKnow/thingsToKnowOhOh.txt")

;; tag tables
;(setq tags-table-list '("/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/django/TAGS"))

;; visit tags
;(visit-tags-table "/Volumes/PwC-VM/TAGS")

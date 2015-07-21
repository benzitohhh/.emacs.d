;; Set default emacs C source directory
;(setq source-directory "/usr/local/emacs/emacs-24.3")

;; Add /usr/local/bin to the path (otherwise emacs can't find stuff installed there)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Git support
(add-to-list 'load-path "/usr/local/git/contrib/emacs")

;; Make sure magit commit uses the existing client (there is a bug where it opens another emacs instance)
(set-variable 'magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

;; full-ack
(setq ack-executable "/usr/local/bin/ack")

;; set default window size
(add-to-list 'default-frame-alist '(width . 104))
(add-to-list 'default-frame-alist '(height . 82))
(add-to-list 'default-frame-alist '(left . 770))
(add-to-list 'default-frame-alist '(top . 255))

;; python executable (comment this out to use tge default)
;;(setq python-shell-interpreter "/usr/local/bin/ipython")
(setq python-shell-interpreter "/usr/local/Cellar/python/2.7.5/bin/python")

;; default directory
(setq default-directory "~/Desktop/")
;(setq default-directory "~/dev/src/eqip")

;; Things to know doc
(setq things-to-know-file "~/Documents/thingsToKnow/thingsToKnowOhOh.txt")

;; tag tables
;;(setq tags-table-list
;;           '("/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/django/TAGS"))

;; visit tags
;(visit-tags-table "/Volumes/PwC-VM/TAGS")

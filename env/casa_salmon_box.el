;; Set default emacs C source directory
(setq source-directory "/usr/src/emacs-24.5-2.src/emacs-24.5")

;; Add /usr/local/bin to the path (otherwise emacs can't find stuff installed there)
;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;(setq exec-path (append exec-path '("/usr/local/bin")))

;; git executable
;; Git is unnnusably slow on emacs/cygwin, because cygwin is slow at starting external processes.
;; Not much that we can do here....
(setq magit-git-executable "/usr/bin/git")

;; Windows workarounds: make sure mouse copies region, and click to paste works
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)

;; Things to know doc
(setq things-to-know-file "~/thingsToKnow/thingsToKnowOhOh.txt")

;; tag tables
;(setq tags-table-list '("/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/django/TAGS"))

;; visit tags
;(visit-tags-table "/Volumes/PwC-VM/TAGS")G

;; Set default emacs C source directory
(setq source-directory "/usr/src/emacs-24.5-2.src")

;; Add /usr/local/bin to the path (otherwise emacs can't find stuff installed there)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; git executable (hmmm seems git is slow on windows/cygwin)
(setq magit-git-executable "/usr/bin/git")

;; tag tables
;(setq tags-table-list '("/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/django/TAGS"))

;; visit tags
;(visit-tags-table "/Volumes/PwC-VM/TAGS")G

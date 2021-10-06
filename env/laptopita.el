;; Set default emacs C source directory
;(setq source-directory "/Applications/emacs-25.1-src")

;; Git support
;(add-to-list 'load-path "/usr/local/git/contrib/emacs")

;; Make sure magit commit uses the existing client (there is a bug where it opens another emacs instance)
;(set-variable 'magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

;; full-ack
(setq ack-executable "/usr/local/bin/ack")

;; set default window size
;(add-to-list 'default-frame-alist '(width . 104))
;(add-to-list 'default-frame-alist '(height . 82))
;(add-to-list 'default-frame-alist '(left . 770))
;(add-to-list 'default-frame-alist '(top . 0))

;; python executable (comment this out to use tge default)
;;(setq python-shell-interpreter "/usr/local/bin/python")
;;(setq python-shell-interpreter "/usr/local/Cellar/python/2.7.5/bin/python")

;; python 3
;(setq python-shell-interpreter "/usr/local/bin/ipython")

;; python2
;; (setq python-shell-interpreter "/usr/local/bin/ipython2"
;;       python-shell-interpreter-args "--simple-prompt -i") ;; for Python 2, need to set simple prompt

;; use current ipython
;(setq python-shell-interpreter "ipython"
;      python-shell-interpreter-args "--simple-prompt -i" ;; disable colours (maybe not necessary anymore???)
;      )

;; python 3  (miniconda)
;(setq python-shell-interpreter "/opt/miniconda3/bin/python")

;; python path
;(setenv "PYTHONPATH" "/Users/benimmanuel/dev/src/cipher/deploy")


;; python jedi environment root
;;(setq jedi:environment-root "/Users/benimmanuel/.virtualenvs/pandas")
;(setq conda-anaconda-home "/opt/miniconda3")

;; default directory
(setq default-directory "~/dev/src/frontend-service/client")
;(setq default-directory "~/dev/src/eqip")

;; Things to know doc
(setq things-to-know-file "~/Documents/thingsToKnow/thingsToKnowOhOh.txt")

;; Aistemos Things to know doc
(setq aistemos-things-to-know-file "~/Desktop/cipherMisc/aistemos_thingsToKnow.txt")

;; tag tables
;;(setq tags-table-list
;;           '("/opt/local/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/site-packages/django/TAGS"))

;; visit tags
;(visit-tags-table "/Volumes/PwC-VM/TAGS")

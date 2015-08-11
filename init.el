;; TODO:
;; webmode

;; Emacs shell reads ~/.bashrc by default, so on OSX do:
;;    ln -s .bash_profile .bashrc

;; Turn off menubar/toolbar/scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Don't display welcome message
(setq inhibit-startup-message t)

;; Color scheme
(if (window-system)
    (progn
      ;(load-theme 'wombat) ;; medium contrast (dark-grey bg)
      (load-theme 'deeper-blue) ;; medium conrtast (dark-blue bg)
      ;(require 'zenburn-theme) ;; low contrast (light grey bg)
      ))

;; Load environment
(load "~/.emacs.d/init-env.el")

;; Marmalade, Melpa package archive
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Make sure the below packages are available
(defvar my-elpa-packages
  '(
    ;;flycheck
    ;;ac-cider
    auto-complete
    browse-kill-ring
    cider
    clojure-mode
    elisp-slime-nav ;; allows M-. to elisp source code
    etags-select
    expand-region
    find-file-in-project
    flx-ido
    full-ack
    git-gutter
    groovy-mode
    idle-highlight
    jedi
    js2-mode
    magit
    markdown-mode
    multiple-cursors
    paredit
    php-mode
    scala-mode
    sparql-mode
    tern
    tern-auto-complete
    visual-regexp
    web-mode
    yasnippet
    zenburn-theme
    ))
(dolist (p my-elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Unix utf8 please
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; When a region is selected, typing will overwrite it
(delete-selection-mode 1)

;; set colours for whitespace-mode
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; Prevent files opened from finder opening in new frame
(setq ns-pop-up-frames nil)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Don't add new lines at end of file
(setq next-line-add-newlines nil)
(setq require-final-newline nil)

;; Set ediff to split vertically (default is horizontal)
(setq ediff-split-window-function 'split-window-horizontally)

;; Git gutter always
(global-git-gutter-mode +1)

;; Ido mode please (with flx - fuzzy matching)
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq flx-ido-use-faces t)

;; Find file in project (do not search the below directories)
(setq ffip-find-options
      (mapconcat
       (lambda (pat) (format "-not -iwholename '%s'" pat))
       '("*/frontend/target/*"
         "*/.target/*"
         "*/.build/*"
         "*/build/*")
       " "))

;; Find file in project (set top-level dir)
;(setq ffip-project-root-function (lambda () "~/dev/src/labs/ui-v2"))

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; full-ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)

;; Saveplace - remembers previous position in a file
(require 'saveplace)
(setq-default save-place t)

;; Yas snippets (uses the default snippets, and my custom snippets from ~/.emacs.d/snippets)
(require 'yasnippet)
(yas-global-mode 1)

;; Auto-complete config
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t) ;; this enables extra keys (for example C-s to filter results)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)
(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))
(dolist (mode '(magit-log-edit-mode yaml-mode
                ;; text-mode
                yaml-mode csv-mode
                html-mode nxml-mode sh-mode clojure-mode
                lisp-mode  markdown-mode))
  (add-to-list 'ac-modes mode))
(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map (kbd "M-RET") 'ac-help)
(define-key ac-completing-map "\r" 'nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscelaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun align-to-equals (begin end)
  "Align region to equal signs"
   (interactive "r")
   (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region
     b e "python -mjson.tool" (current-buffer) t)))

(defun open-init ()
  "Open bindings.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-things-to-know ()
  "Open thingsToKnow.txt"
  (interactive)
  (find-file things-to-know-file)) ;; this var is defined in env file

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;

;; To cycle between frames
(global-set-key "\M-`" 'other-frame)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors
(global-set-key (kbd "<f1>") 'set-rectangular-region-anchor)
(global-set-key (kbd "<f5>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f6>") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "<f7>") 'mc/mark-all-like-this)

;; Find files
(global-set-key (kbd "<f13>") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "<f14>") 'recentf-open-files)

;; ack
(global-set-key (kbd "<f15>") 'ack) ;; By default, ack takes a regex. To pass it a literal, use C-u prefix
                                    ;; Also don't forget... ack-same. This restricts results to files of type associated with current mode

;; Magit
(global-set-key (kbd "s-r") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f16>") 'magit-diff)
(global-set-key (kbd "<f17>") 'magit-log)

;; shortcuts for some useful files
(global-set-key (kbd "<f18>") 'open-init)
(global-set-key (kbd "<f19>") 'open-things-to-know)

(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point)

(global-set-key (kbd "C-c n") 'delete-trailing-whitespace)

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

(global-set-key (kbd "C-x a") 'align-to-equals)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE-SPECIFIC SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-coding-hook ()
  "Stuff to apply when coding"
  (if (window-system)
      (idle-highlight t)) ;; idle-highlight looks weird on the commandline
  (electric-pair-mode 1))

(defun set-indent-level-web (size)
  "Set the indent level for js/html/css"
  (interactive "nEnter indent level (for js/html/css): ")
  (setq js-indent-level size)
  (setq js2-basic-offset size)
  (setq sgml-basic-offset size))

;; css
(add-hook 'css-mode-hook 'my-coding-hook)

;; html
(setq sgml-basic-offset 2)
(add-hook 'sgml-mode-hook 'my-coding-hook)

;; javascript
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t) ;; hmmmm maybe this should be t? Or nil?
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json" . js-mode))
(add-hook 'js-mode-hook 'my-coding-hook)
(add-hook 'js2-mode-hook 'my-coding-hook)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
;; Make sure there is a default ".tern-config" in home directory...
;;   {
;;     "plugins": {
;;       "node": {}
;;     }
;;   }

;; Php
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(add-hook 'php-mode-hook 'my-coding-hook)

;; Web mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; Groovy
(add-to-list 'auto-mode-alist '("\\.gradle" . groovy-mode))
(add-hook 'groovy-mode-hook 'my-coding-hook)

;; Scala
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))
(add-hook 'scala-mode-hook 'my-coding-hook)

;; Shell script
(add-to-list 'auto-mode-alist '("\\routes$" . shell-script-mode))
(add-hook 'shell-script-mode-hook 'my-coding-hook)

(defun my-lispy-coding-hook ()
  "Stuff to apply when coding lispy languages"
  (turn-on-elisp-slime-nav-mode) ;; hmmm perhaps only add this for elisp (not clojure)
  (show-paren-mode)
  (enable-paredit-mode))

;; EmacsLisp
(add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lispy-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; Clojure
(add-hook 'clojure-mode-hook 'my-coding-hook)
(add-hook 'clojure-mode-hook 'my-lispy-coding-hook)

;; (defun my-cider-setup ()
;;   "Stuff to apply if using cider"
;;   (remove-hook 'nrepl-connected-hook 'cider-display-connected-message) ;; remove annoying startup message
;;   (require 'ac-cider-compliment)
;;   (add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;;   (eval-after-load "auto-complete"
;;     '(add-to-list 'ac-modes cider-mode))

;; Python
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (newline-and-indent)
  (annotate-pdb))
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
	    (my-coding-hook)
            (define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)
            (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
            (define-key python-mode-map (kbd "M->") 'jedi:goto-definition-pop-marker)
            (annotate-pdb)))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(setq octave-block-offset 4)
(add-hook 'octave-mode-hook
	  (lambda ()
	    (my-coding-hook)
	    (define-key octave-mode-map (kbd "C-x C-e") 'octave-send-line)
	    (define-key octave-mode-map (kbd "C-M-x") 'octave-send-block)))

;; Sparql
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))
(add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode))
(add-hook 'sparql-mode-hook
          (lambda ()
	    (my-coding-hook)
            (sparql-set-base-url "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&format=csv&timeout=30000&debug=on")
            (define-key sparql-mode-map (kbd "C-c C-c") 'sparql-query-region)
            (define-key sparql-mode-map (kbd "<s-return>") 'sparql-query-region)))

;;;;;;;;;;;;;;;;;;;
;; Tempoarary
;;;;;;;;;;;;;;;;;;;

;; Example of calling a shell command
(global-set-key (kbd "<s-return>") 'wa-annotate)
(defvar wa-input "input/drugFams.csv")
(defvar wa-n 10)
(defvar wa-output "out.html")
(defun wa-annotate ()
  "Call wikipedia annotate script"
  (interactive)
  (shell-command
   (concat "cd /Users/benimmanuel/Desktop/wikipedia-annotate; "
           "python annotate.py " wa-input " -n " (number-to-string wa-n)
           " > " wa-output)))

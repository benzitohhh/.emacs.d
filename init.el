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

;; Set font size
;;;; M-x describe-font to see current font string
;; (set-frame-font "-*-Menlo-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1" nil t)
;; (add-to-list 'default-frame-alist '(font . "-*-Menlo-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1" ))
;; (set-face-attribute 'default t :font "-*-Menlo-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1" )

;; Highlight current line (can look weird with modes that highlight text, such as rainbow-mode, magit etc...)
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#222536")

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
    exec-path-from-shell
    find-file-in-project
    flx-ido
    full-ack
    git-gutter
    groovy-mode
    haskell-mode
    idle-highlight-mode
    jedi
    js-doc
    js2-mode
    less-css-mode
    magit
    markdown-mode
    multiple-cursors
    paredit
    php-mode
    rainbow-mode
    scala-mode
    sparql-mode
    tern
    tern-auto-complete
    virtualenvwrapper
    visual-regexp
    visual-regexp-steroids
    web-mode
    whitespace-cleanup-mode
    yasnippet
    zenburn-theme
    ))
(dolist (p my-elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Add /usr/local/bin to front of the path and exec-path (otherwise emacs can't find stuff installed there)
;; (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; (add-to-list 'exec-path "/usr/local/bin")

;; On MacOXS, get environment variables from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Unix utf8 please
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; set colours for whitespace-mode (and which things whitespace-cleanup should clean)
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark trailing empty)))

;; Fix whitespace on save, but only if the file was clean
(global-whitespace-cleanup-mode)

;; When a region is selected, typing will overwrite it
(delete-selection-mode 1)

;; Prevent files opened from finder opening in new frame
(setq ns-pop-up-frames nil)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Don't add new lines at end of file
(setq next-line-add-newlines nil)
(setq require-final-newline nil)

;; Set ediff to split vertically (default is horizontal)
(setq ediff-split-window-function 'split-window-horizontally)

;; Set window to split vertically by default
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 80)

;; No backup files please (those filename.ext~ files)
(setq make-backup-files nil)

;; Git gutter always
(global-git-gutter-mode +1)

;; Magit
(setq magit-diff-refine-hunk 'all) ;; show word-level diffs
(setq magit-push-always-verify nil) ;; no verify please
(setq magit-status-buffer-switch-function 'switch-to-buffer) ;; open magit-status in a full window

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

;; Find file in project: do not search the below directories
(setq ffip-find-options
      (mapconcat
       (lambda (pat) (format "-not -iwholename '%s'" pat))
       '("*/frontend/target/*"
         "*target/scala-*"
         "*target/universal*"
         "*/.target/*"
         "*/.build/*"
         "*/build/*")
       " "))
;; For per-project settings, use 'per-directory' local variables: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables
;; Specifically...
;;   add a '.dir-locals.el' file to a dir, containing something like:
;;      ((nil . ((ffip-project-root . "~/Desktop/labs/ui-v2"))))

;; Use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Find file in project: use ido mode ui
(setq ffip-prefer-ido-mode t)

;; Find file in project (set top-level dir)
;(setq ffip-project-root-function (lambda () "~/dev/src/labs/ui-v2"))

;; Grep default
(require 'grep)
(grep-apply-setting 'grep-command "grep -nHR --include \\*.js -e somePattern .")

;; full-ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(setq ack-prompt-for-directory t) ;; always ask for directory before doing an ack search

;; Visual regex
(require 'visual-regexp)
;(require 'visual-regexp-steroids) ;; for Python-style regex
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark) ;; for multiple cursors

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

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
      (idle-highlight-mode t)) ;; idle-highlight looks weird on the commandline
  (electric-pair-mode 1))

(defun set-indent-level-web (size)
  "Set the indent level for js/html/css"
  (interactive "nEnter indent level (for js/html/css): ")
  (setq js-indent-level size)
  (setq js2-basic-offset size)
  (setq sgml-basic-offset size))

;; C and C++
(setq c-basic-offset 4)
(c-set-offset 'case-label '+)
(setq compile-command "make -k -C pathToWorkingDir clean all; ctags -e -R;")
(add-hook 'c-mode-common-hook
          (lambda ()
	    (my-coding-hook)
            (setq comment-start "//" comment-end "") ;; comments with //, not /* */
            ;; When compile is called, it asks for a compile command...
            ;;   Simple compile command: "make -k"
            ;;   More complex compile command: "ctags -e -R; make clean; make -k ex17; ./ex17;"
            (define-key c-mode-map (kbd "C-x x") 'compile)
            (define-key c-mode-map (kbd "M-RET") 'compile)
            (define-key c-mode-map (kbd "<s-return>") 'compile)))

;; Makefile-mode
(add-hook 'makefile-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (setq-local indent-tabs-mode t) ;; Make needs to use tabs, not spaces
            (setq-local tab-width 4)))

;; css
(add-hook 'css-mode-hook 'my-coding-hook)
(add-hook 'css-mode-hook (lambda () (rainbow-mode t)))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; html
(setq sgml-basic-offset 2)
(add-hook 'sgml-mode-hook 'my-coding-hook)

;; javascript (i.e. js2-mode also sets js-mode, so only need to bind stuff to js-mode)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t) ;; hmmmm maybe this should be t? Or nil?
(setq js2-indent-switch-body t) ;; indent switch statements nicely
(setq js2-strict-missing-semi-warning nil) ;; set to true to show errors if semicolons are missing
;; (set-face-attribute 'js2-function-call nil :foreground "light goldenrod") ;; hmmm probably not useful
;; (set-face-attribute 'js2-object-property nil :foreground "pink") ;; hmmm probably not useful
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode)) ;; use this for js (no jsx)
;; (add-to-list 'auto-mode-alist '("\\.js" . js2-jsx-mode)) ;; use this for jsx
(add-to-list 'auto-mode-alist '("\\.jsx" . js2-jsx-mode)) ;; use this for jsx
(add-to-list 'auto-mode-alist '("\\.json" . js-mode))
(add-hook 'js-mode-hook 'my-coding-hook)
(add-hook 'js-mode-hook
          (lambda ()
            (tern-mode t)
            (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
            (define-key js2-mode-map "@" 'js-doc-insert-tag)))


(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
;; Make sure there is a default ".tern-config" in home directory...
;;   {
;;     "plugins": {
;;       "node": {},
;;       "es_modules": {}
;;     }
;;   }
(defun tern-delete-process ()
  (interactive)
  (if (get-process "Tern")
      (delete-process "Tern")))

(defun tern-debug ()
  "On the commandline:  killall node; tern --verbose --port 50888. Then call this function."
  (interactive)
  (tern-delete-process)
  (tern-use-server 50888 "127.0.0.1"))

;; To debug tern:
;;   a) M-x tern-debug
;;   b) Start server on commandline:  killall node; tern --verbose --port 50888
;;   c) Do stuff

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
(define-key emacs-lisp-mode-map (kbd "C-x C-j") 'eval-print-last-sexp)
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
(require 'virtualenvwrapper) ;; to switch to a virtualenv, M-x venv-workon -> JEDI and shell pick this up
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
            (electric-pair-mode 0)
            (define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)
            (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
            (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
            (annotate-pdb)))

;; Haskell
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-simple-indent)))

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
;(global-set-key (kbd "<s-return>") 'wa-annotate)
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

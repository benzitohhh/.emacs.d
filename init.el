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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))  ;; hmmm sometimes marmalade is offline...
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; NOTE: if you have problems, try doing M-x package-refresh-contents (or M-x package-list-packages)
(package-initialize)

;; To use local rjsx-mode.el
;; a) M-x package-delete (delete rjsx-mode)
;; b) comment our rjsx-mode in the melpa list below
;; c) uncomment the below line, loading it from a file somewhere
;(load "/Users/benimmanuel/Desktop/rjsx-mode/rjsx-mode.el")

;; Make sure the below packages are available
(defvar my-elpa-packages
  '(
    ag
    auto-complete
    company
    conda
    dockerfile-mode
    dumb-jump
    elisp-slime-nav ;; allows M-. to elisp source code
    expand-region
    exec-path-from-shell
    find-file-in-project
    flx-ido
    flycheck
    full-ack
    git-gutter
    glsl-mode
    ido-completing-read+
    idle-highlight-mode
    jedi
    js-doc
    js-import
    js2-mode
    less-css-mode
    magit
    markdown-mode
    multiple-cursors
    paredit
    php-mode
    projectile
    protobuf-mode
    rainbow-mode
    rjsx-mode
    rust-mode
    tide
    virtualenvwrapper
    visual-regexp
    visual-regexp-steroids
    web-mode
    whitespace-cleanup-mode
    yaml-mode
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
;(set-default default-buffer-file-coding-system 'utf-8-unix)

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
(setq mode-require-final-newline nil)

;; Show column number please
(setq column-number-mode t)

;; Set ediff to split vertically (default is horizontal)
(setq ediff-split-window-function 'split-window-horizontally)

;; Set window to split vertically by default
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 80)

;; No backup files please (those filename.ext~ files)
(setq make-backup-files nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company tide ido-completing-read+ conda protobuf-mode zenburn-theme yasnippet whitespace-cleanup-mode web-mode visual-regexp-steroids visual-regexp virtualenvwrapper rainbow-mode php-mode paredit multiple-cursors markdown-mode magit less-css-mode js2-mode js-doc jedi idle-highlight-mode haskell-mode glsl-mode git-gutter full-ack flycheck flx-ido find-file-in-project exec-path-from-shell expand-region elisp-slime-nav dumb-jump dockerfile-mode auto-complete))
 '(safe-local-variable-values
   '((ffip-project-root . "/Users/benimmanuel/dev/src/cipher/frontend")))
 '(sort-fold-case t t))

;; ffip - to initialise a new project, run M-x ffip-create-project-file    - this drops a .dir-locals.el file, allowing for search with multiple nested git subprojectrs or modules.

;; Git gutter always
(global-git-gutter-mode +1)

;; Magit
(setq magit-diff-refine-hunk 'all) ;; show word-level diffs
;(setq magit-push-always-verify nil) ;; no verify please
;(setq magit-status-buffer-switch-function 'switch-to-buffer) ;; open magit-status in a full window

;; Ido mode please (with flx - fuzzy matching)
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
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
         "*/dist/*"
         "*/build/*"
         "*/site-packages/*")
       " "))

;; Find file in project: remove "bin" from ignore list
(require 'find-file-in-project)
(setq ffip-prune-patterns (cl-remove-if (lambda (x) (equal x "*/bin/*")) ffip-prune-patterns))

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
(require 'full-ack)
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
;(add-to-list 'ack-project-root-file-patterns "frontend-service") ;; regex for default directory to search
(add-to-list 'ack-project-root-file-patterns ".gitignore") ;; regex for default directory to search
(setq ack-prompt-for-directory t) ;; always ask for directory before doing an ack search
(defun ack-at-point ()
  (interactive)
  (ack (thing-at-point 'symbol) nil (ack-guess-project-root)))

;; Visual regex
(require 'visual-regexp)
;(require 'visual-regexp-steroids) ;; for Python-style regex
(define-key global-map (kbd "C-c r") 'vr/replace)

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

;; Dumb jump mode always
(dumb-jump-mode)
(add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
(define-key global-map (kbd "C-M-f") 'dumb-jump-go-prefer-external) ;; useful for jumping to references

;; Auto-complete config
(ac-config-default)
;(global-auto-complete-mode t)
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
                yaml-mode
                html-mode sh-mode
                lisp-mode markdown-mode))
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

(defun open-aistemos-things-to-know ()
  "Open aistemos_thingsToKnow.txt"
  (interactive)
  (find-file aistemos-things-to-know-file)) ;; this var is defined in env file

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
;(global-set-key (kbd "<C-S-right>") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors
(global-set-key (kbd "<f1>") 'set-rectangular-region-anchor)
(global-set-key (kbd "<f5>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f6>") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "<f7>") 'mc/mark-all-like-this)

;; Find file / Ack
(global-set-key (kbd "<f13>") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "<f14>") 'ack-at-point)
(global-set-key (kbd "<f15>") 'ack) ;; By default, ack takes a regex. To pass it a literal, use C-u prefix
                                    ;; Also don't forget... ack-same. This restricts results to files of type associated with current mode
(global-set-key (kbd "<f16>") 'recentf-open-files)

;; Magit
(global-set-key (kbd "s-r") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)

;; shortcuts for some useful files
(global-set-key (kbd "<f17>") 'open-init)
(global-set-key (kbd "<f18>") 'open-aistemos-things-to-know)
(global-set-key (kbd "<f19>") 'open-things-to-know)

(global-set-key (kbd "C-c n") 'delete-trailing-whitespace)

(global-set-key (kbd "C-x a") 'align-to-equals)

;; Shortcut for wiping buffer
(global-set-key (kbd "s-k") 'erase-buffer)
(put 'erase-buffer 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE-SPECIFIC SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-coding-hook ()
  "Stuff to apply when coding"
  ;; (if (window-system)
  ;;     (idle-highlight-mode t)) ;; idle-highlight looks weird on the commandline
  (electric-pair-mode 1)
  (hl-line-mode 1) ;; highlight current line
  (set-face-background 'hl-line "#1f1b2f") ;; but make sure highlight current line is subtle
  )

(defun set-indent-level-web (size)
  "Set the indent level for js/html/css"
  (interactive "nEnter indent level (for js/html/css): ")
  (setq js-indent-level size)
  (setq js2-basic-offset size)
  (setq sgml-basic-offset size)
  (setq css-indent-offset size)
  (setq web-mode-markup-indent-offset size)
  (setq web-mode-code-indent-offset size)
  (setq web-mode-script-padding size)
  (setq web-mode-attr-indent-offset size))

(defun tabs-please ()
  "Use tabs rather than spaces"
  (interactive)
  (setq-default indent-tabs-mode t)
  (web-mode-use-tabs))


;; Protobuffer
(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; C and C++
(setq c-basic-offset 4)
(c-set-offset 'case-label '+)
;(setq compile-command "make -k -C pathToWorkingDir clean all && ctags -e -R;")
;(setq compile-command "make clean && make ex1 && echo && ./ex1;")
(setq compile-command "make clean all; ctags -e -R")
(add-hook 'c-mode-common-hook
          (lambda ()
	    (my-coding-hook)
            (flycheck-mode +1)
            (setq comment-start "//" comment-end "") ;; comments with //, not /* */
            ;; When compile is called, it asks for a compile command...
            ;;   Simple compile command: "make -k"
            ;;   More complex compile command: "ctags -e -R; make clean; make -k ex17; ./ex17;"
            (define-key c-mode-map (kbd "C-x x") 'compile)
            (define-key c-mode-map (kbd "M-RET") 'compile)
            (define-key c-mode-map (kbd "<s-return>") 'compile)
            (define-key c-mode-map (kbd "M-,") 'pop-tag-mark)
            (define-key c-mode-map (kbd "C-x m") 'man))
          )

;; GLSL
(add-hook 'glsl-mode-hook
          (lambda ()
	    (my-coding-hook)
            (auto-complete-mode))
          )
(add-to-list 'auto-mode-alist '("\\.glsl$" . glsl-mode))

;; Makefile-mode
(add-to-list 'auto-mode-alist '("\\Makefile" . makefile-mode))
(add-hook 'makefile-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (setq-local indent-tabs-mode t) ;; Make needs to use tabs, not spaces
            (setq-local tab-width 4)))

;; css
(setq css-indent-offset 2)
(add-hook 'css-mode-hook 'my-coding-hook)
(add-hook 'css-mode-hook (lambda () (rainbow-mode t)))
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;; html
(setq sgml-basic-offset 2)
(add-hook 'sgml-mode-hook 'my-coding-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst js-file-extension-regex "\\.[jt]sx?$")

(defun get-css-module-file ()
  "Get sass css module file that corresponds to current file (assumes current file is a .js file)."
  (replace-regexp-in-string js-file-extension-regex ".module.css" (buffer-file-name)))

(defun get-scss-module-file ()
  "Get sass css module file that corresponds to current file (assumes current file is a .js file)."
  (replace-regexp-in-string js-file-extension-regex ".module.scss" (buffer-file-name)))

(defun open-scss-module ()
  "Open sass css-module that corresponds to current file."
  (interactive)
  (find-file (get-scss-module-file)))

(defun create-scss-module ()
  "Create sass css-module based on current filename, add imports for it (assumes current file is a .js file)."
  (interactive)
  (let ((scss-file (get-scss-module-file))
        (css-file (get-css-module-file)))
    ;; add import to current buffer
    (insert "import cn from 'classnames'")
    (newline)
    (insert (concat "import s from './" (file-name-nondirectory css-file) "'"))
    (newline)
    (insert "// <div className={cn(s.button, {[s.button__active]: is_clicked})} />")
    (newline)
    (save-buffer)
    ;; create file if it does not exist
    (shell-command (concat "touch " scss-file))
    ;; open the file
    (find-file scss-file)))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun my-js-hook ()
  (interactive)
  (global-set-key (kbd "C-c i") 'js-doc-insert-function-doc) ;; collision
  (global-set-key (kbd "@")     'js-doc-insert-tag)
  (global-set-key (kbd "C-c m") 'create-scss-module)
  (global-set-key (kbd "C-c c") 'open-scss-module)
  (global-set-key (kbd "C-c v") 'toggle-camelcase-underscores)
  (global-set-key (kbd "s-i")    'js-import))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)

;; By default, set all js/html/css indents to 2
(set-indent-level-web 2)

;; For js import, use single quote
(setq js-import-quote "\'")
(setq projectile-git-submodule-command nil) ;; currently projectile can't deal with submodules (projectile used by js-import)

;; js / jsx - use rjsx mode
(add-to-list 'auto-mode-alist '("\\.js" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx" . rjsx-mode))

;; ts / tsx - use web mode
(add-to-list 'auto-mode-alist '("\\.ts" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx" . web-mode))

;; json - use js-mode
(add-to-list 'auto-mode-alist '("\\.json" . js-mode))                                        ;(add-to-list 'auto-mode-alist '("component.*\\/.*\\.js\\'" . rjsx-mode)) ;; use this for jsx

;; js-mode
(add-hook 'js-mode-hook
          (lambda ()
            (my-coding-hook)
            (my-js-hook)
            (auto-complete-mode t)
            ))

;; js2-mode (extends js-mode)
(setq js2-bounce-indent-p t)
(setq js2-indent-switch-body t) ;; indent switch statements nicely
(setq js2-strict-missing-semi-warning nil) ;; set to true to show errors if semicolons are missing
(add-hook 'js2-mode-hook
          (lambda ()
            (setup-tide-mode)
            ))

;; js2-mode + flycheck....
;; To get Flycheck to use jsx-tide with .js files, need to override the checker like this
;; For now this is disabled (i.e. commented out below) as it flags up too many warnings. TODO: investigate.
;; Meanwhile... just uing eslint
;; (flycheck-define-generic-checker 'jsx-tide
;;   "A JSX syntax checker using tsserver."
;;   :start #'tide-flycheck-start
;;   :verify #'tide-flycheck-verify
;;   :modes '(web-mode js2-jsx-mode rjsx-mode)
;;   :predicate (lambda ()
;;                (and
;;                 (tide-file-extension-p "js")
;;                 (tide-flycheck-predicate))))

;; Web mode
;(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (set-face-foreground 'web-mode-html-tag-face "Orange3")
            (set-face-foreground 'web-mode-html-attr-name-face "seagreen1")
            (set-face-foreground 'web-mode-function-call-face "lightgoldenrod3")

            (setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
            (add-to-list 'web-mode-comment-formats '("javascript" . "//"))

            (my-coding-hook)
            (my-js-hook)

            (setup-tide-mode)

            ;; configure jsx-tide checker to run after your default jsx checker
            (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
            )
          )

;; Php
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(add-hook 'php-mode-hook 'my-coding-hook)

;; Shell script
(add-to-list 'auto-mode-alist '("\\routes$" . shell-script-mode))
(add-hook 'shell-script-mode-hook 'my-coding-hook)

(defun my-lispy-coding-hook ()
  "Stuff to apply when coding lispy languages"
  (turn-on-elisp-slime-nav-mode)
  (show-paren-mode)
  ;;(enable-paredit-mode)
)

;; EmacsLisp
(add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lispy-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(define-key emacs-lisp-mode-map (kbd "C-x C-j") 'eval-print-last-sexp)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(require 'virtualenvwrapper) ;; to switch to a virtualenv, M-x venv-workon -> JEDI and shell pick this up
;(require 'conda)
;; i.e. need something like (setq conda-anaconda-home "/Users/benimmanuel/miniconda3")
;; i.e. then to activate an env, M-x conda-env-activate <ret> py2
;(conda-env-activate "py3")
;; ALSO: see ./env/ folder for pointing jedi home to different python envs (usefulf for seeing 3rd party source code)
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
(defun my-python-shell-send-region-or-line ()
  "Send the current region (or whole line if none selected) to the python shell.
For some reason python-shell-send-region does not show the result in
the shell, hence this workaround."
  (interactive)
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((string (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line))))
            (process (python-shell-get-or-create-process)))
        (message "Sent: %s..." string)
        (python-shell-send-string string process)))))
;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(defun flycheck-python-setup ()
  (flycheck-mode))
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (flycheck-python-setup)
            (idle-highlight-mode)
            (electric-pair-mode 1) ;; set to 0 to disable electric pair
            (define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)
            (define-key python-mode-map (kbd "C-x C-e") 'my-python-shell-send-region-or-line)
            (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
            (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
            (define-key python-mode-map (kbd "C-x x") 'compile)
            (define-key python-mode-map (kbd "M-RET") 'compile)
            (define-key python-mode-map (kbd "<s-return>") 'compile)
            (annotate-pdb)))

(add-hook `inferior-python-mode-hook
          (lambda ()
            (local-set-key (kbd "s-k") 'comint-clear-buffer)))


;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Yaml
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))

;; Conf
(add-to-list 'auto-mode-alist '("cipher\-config\-" . conf-mode))
;;(add-to-list 'auto-mode-alist '("cipher\-conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("cipher\-conf$" . conf-mode))
(add-to-list 'auto-mode-alist '("\.env$" . conf-mode))
(add-to-list 'auto-mode-alist '("\.env_sample$" . conf-mode))

(add-to-list 'auto-mode-alist '("\.cli$" . shell-script-mode))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

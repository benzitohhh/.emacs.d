;; TODO:
;; bindings.el (just language-specific stuff to go now...)
;; appearance:
;;   see https://github.com/tmtxt/.emacs.d/blob/master/config/tmtxt-appearance.el
;; !!! load environment file
;; tern-mode
;; python (JEDI)
;; autocomplete - make sure it works everywhere!
;; Also check... epl, find-file-in-project (and flx)
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

;; Marmalade, Melpa package archive
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Make sure the below packages are available
(defvar benimmanuel/elpa-packages
  '(
    visual-regexp
    ;;flycheck
    ;;css-eldoc
    ;;nodejs-repl
    ;;restclient
    ;;highlight-escape-sequences
    ;popup
    ;ac-cider-compliment
    ;jedi
    ;; concurrent
    ;; ctable
    ;; deferred
    ;; epc
    auto-complete
    browse-kill-ring
    web-mode
    elisp-slime-nav ;; allows M-. to elisp source code
    expand-region
    multiple-cursors
    git-gutter
    yasnippet
    auto-complete
    paredit
    magit
    groovy-mode
    scala-mode
    etags-select
    full-ack
    idle-highlight
    php-mode
    js2-mode
    sparql-mode
    zenburn-theme
    tern
    tern-auto-complete
    markdown-mode
    ))
(dolist (p benimmanuel/elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Unix utf8 please
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

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

;; Saveplace always - remembers previous position in a file
(require 'saveplace)
(setq-default save-place t)

;; Yas snippets always (uses the default snippets, and my custom snippets from ~/.emacs.d/snippets)
(require 'yasnippet)
(yas-global-mode 1)

;; Electric pair mode always please
(electric-pair-mode 1)

;; Git gutter always
(global-git-gutter-mode +1)

;; full-ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; Auto-complete config
(ac-config-default)

;;ac-dictionary-diectories

;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;

;; To cycle between frames
(global-set-key "\M-`" 'other-frame)

;; find file (prompts for start dir, then searches recursively)
(global-set-key "\C-x\ f" 'find-name-dired)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors
(global-set-key (kbd "<f1>") 'set-rectangular-region-anchor)
(global-set-key (kbd "<f5>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f6>") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "<f7>") 'mc/mark-all-like-this)

;; ack
(global-set-key (kbd "<f13>") 'ack-find-file)
(global-set-key (kbd "<f15>") 'ack)

;; Magit
(global-set-key (kbd "s-r") 'magit-status)
(global-set-key (kbd "<f16>") 'magit-diff)
(global-set-key (kbd "<f17>") 'magit-log)

;; Ensure magit uses HEAD as default for diff
(defadvice magit-diff (before magit-diff-default-to-head activate)
  "Offer HEAD as first default for magit-diff"
  (interactive (list (magit-read-rev-range "Diff" "HEAD"))))

;; open init file
;; (global-set-key (kbd "<f18>") 'open-init)
;; (defun open-init ()
;;   "Open bindings.el"
;;   (interactive)
;;   (find-file init-file))

;; things to know
;; (global-set-key (kbd "<f19>") 'open-things-to-know)
;; (defun open-things-to-know ()
;;   "Open thingsToKnow.txt"
;;   (interactive)
;;   (find-file things-to-know-file))

;; etags-select-find-tag-at-point
(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point)

;; cleanup
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Revert all buffers
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

;; Hack for indents
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

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

;; align to equals
(defun align-to-equals (begin end)
  "Align region to equal signs"
   (interactive "r")
   (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))
(global-set-key (kbd "C-x a") 'align-to-equals)

;; beautify json
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region
     b e "python -mjson.tool" (current-buffer) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE-SPECIFIC SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: create a seperate file for each of these (so they can be turned on/off)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; set indent-level for html
(setq sgml-basic-offset 2)

;; set indent-level for js
(setq js-indent-level 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" default)))
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil))

;; javascript
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json" . js-mode))

;; php-mode
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module" . php-mode))

;; TODO: move this to a java config
;; (require 'cl)
;; (require 'groovy-mode)
;; (add-to-list 'auto-mode-alist '("\\.gradle" . groovy-mode))

;; Markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

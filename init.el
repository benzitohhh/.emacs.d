;; Emacs shell reads ~/.bashrc by default, so on OSX do:
;;    ln -s .bash_profile .bashrc

;; Turn off menubar/toolbar/scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Don't display welcome message
(setq inhibit-startup-message t)

;; add Marmalade, Melpa package archive
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;;; package.el
;;; everytime emacs starts, it will automatically check if those packages are
;;; missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar tmtxt/elpa-packages
  '(
    ;;visual-regexp
    ;;flycheck
    ;;css-eldoc
    ;;nodejs-repl
    ;;restclient
    ;;highlight-escape-sequences
    ;popup
    ;ac-cider-compliment
    ;sparql-mode
    ;jedi
    ;; concurrent
    ;; ctable
    ;; deferred
    ;; epc
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
    zenburn-theme
    tern
    tern-auto-complete
    markdown-mode
    ))
(dolist (p tmtxt/elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; zenburn-theme
(require 'zenburn-theme)

;; Unix utf8 please
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; set colours for whitespace-mode
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; full-ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; prevent files opened from finder opening in new frame
(setq ns-pop-up-frames nil)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Don't add new lines at end of file
(setq next-line-add-newlines nil)
(setq require-final-newline nil)

;; Set ediff to split vertically (default is horizontal)
(setq ediff-split-window-function 'split-window-horizontally)

;; TODO:
;; bindings.el (just language-specific stuff to go now...)
;; appearance:
;;   see https://github.com/tmtxt/.emacs.d/blob/master/config/tmtxt-appearance.el
;; !!! load environment file
;; yas (including default snippets, and custom snippets)
;; js2
;; 
;; browsekillring
;; saveplace
;; python (JEDI)
;; autocomplete - make sure it works everywhere!
;; Also check... epl, find-file-in-project (and flx)


;(setq custom-yasnippet-dir (concat (file-name-directory load-file-name) "etc/snippets"))
;(setq yas/snippet-dirs (cons custom-yasnippet-dir yas/snippet-dirs))
;(yas/reload-all)

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

;; Autocomplete
(global-set-key (kbd "C-.") 'auto-complete)
(global-set-key (kbd "C-`") 'ac-expand)

;; ack
(global-set-key (kbd "<f13>") 'ack-find-file)
(global-set-key (kbd "<f15>") 'ack)

;; git
(global-set-key (kbd "s-r") 'magit-status)
(global-set-key (kbd "<f16>") 'magit-diff)
(global-set-key (kbd "<f17>") 'magit-log)

;; Enable git gutter mode
(global-git-gutter-mode +1)

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
 '(js2-basic-offset 2)  
 '(js2-bounce-indent-p nil)  
)

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

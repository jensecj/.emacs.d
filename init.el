;; =======================================
;; ====================== Interface tweaks
;; =======================================

;; Hide the splash screen
(setq inhibit-startup-message t)

;; Turn off excess interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; =======================================
;; ===================== Setup directories
;; =======================================

;; Path to melpa installed packages
(setq elpa-dir
	(expand-file-name "elpa" user-emacs-directory))

;; Add melpa installed packages to load-path
(dolist (project (directory-files elpa-dir t "\\w+"))
	(when (file-directory-p project)
		(add-to-list 'load-path project)))

;; Add .emacs.d to load-path
(add-to-list 'load-path user-emacs-directory)

;; Keep emacs custom settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Save backup and autosave files in data folder
(defconst storage-dir "~/.emacs.d/data/")
(setq backup-directory-alist
	`((".*" . ,storage-dir)))
(setq auto-save-file-name-transforms
	`((".*" ,storage-dir t)))
(setq auto-save-list-file-prefix
	storage-dir)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "~/.emacs.d/data/.places" user-emacs-directory))

;; =======================================
;; ======================== Setup packages
;; =======================================

; Add melpa as a package archive
(require 'package)
(add-to-list 'package-archives
	'("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install dash, which is needed for the setup-package script
(require 'package)
(package-initialize)
(package-refresh-contents)

(when (not (package-installed-p 'dash))                                                                                              
                (package-install 'dash))

;; Install missing packages
(require 'setup-package)
(defun init--install-packages ()
  (packages-install
   '(magit
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     git-gutter+
     move-text
     visual-regexp-steroids
     smartparens
     flx
     flx-ido
     ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     highlight-escape-sequences
     whitespace-cleanup-mode
     smooth-scrolling
     smex
     expand-region
     smart-forward
     ace-jump-mode
     multiple-cursors
     sml-mode)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; =======================================
;; ====================== Tweaks and hacks
;; =======================================

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(smartparens-global-mode t)

;; Language specific setup files
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Map files to modes
(require 'mode-mappings)

;; load packages
(require 'expand-region)
(require 'multiple-cursors)
(require 'smart-forward)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Load custom keybindings
(require 'key-bindings)

;; =======================================
;; ====================== Interface tweaks
;; =======================================

;; Define our directories
(defconst root-dir "~/.emacs.d/")
(defconst data-dir (concat root-dir "data/"))
(defconst backup-dir (concat data-dir "backups/"))

(unless (file-exists-p data-dir)
  (make-directory data-dir))

(unless (file-exists-p backup-dir)
  (make-directory backup-dir))

;; Hide the splash screen
(setq inhibit-startup-message t)

;; Turn off excess interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Use Source Code Pro font if it is available
(add-to-list 'default-frame-alist '(font . "Source Code Pro Semibold 10"))

;; =======================================
;; ================ Setup data directories
;; =======================================

;; Add .emacs.d to load-path
(add-to-list 'load-path root-dir)

;; Keep emacs custom settings in a separate file
(setq custom-file (concat root-dir "custom.el"))
(load custom-file)

;; Save backup and autosave files in data folder
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
(setq auto-save-list-file-prefix backup-dir)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(require 'tramp)
(setq tramp-persistency-file-name (concat data-dir ".tramp"))

;; stop leaking information, you are not a browser
(require 'url)
(setq url-privacy-level 'paranoid)
(url-setup-privacy-info)
(setq url-temporary-directory data-dir)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat data-dir ".places"))

;; =======================================
;; ====================== Install packages
;; =======================================

(require 'package)

;; Add melpa as a package archive
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install missing packages
(require 'package-helper)
(install-packages
 '(
   ;; Libraries
   s                       ; string manipulation library
   dash                    ; list library
   ;; Minor modes
   git-commit-mode         ; new commit mode
   gitconfig-mode          ; minor mode for editing git config files
   gitignore-mode          ; minor mode for editing git ignore files
   ido-vertical-mode       ; print ido vertically
   ace-jump-mode           ; jump around the buffer with ease
   smartparens             ; automatically add end parens, highlight matching parens
   git-gutter+             ; mark added/changes/removed lines in the gutter
   flycheck                ; linting
   smooth-scrolling        ; add a top and bottom margin when scrolling
   ;; Major modes
   ;; Themes
   zenburn-theme           ; the great zenburn theme
   ;; Packages
   use-package             ; pretty package initialization
   visual-regexp-steroids  ; better regular expressions
   auctex                  ; laTeX editing
   browse-kill-ring        ; browse the kill ring
   change-inner            ; easily change the inner or outer content of something
   flx                     ; flexible matching
   flx-ido                 ; flexible matching in ido mode
   ido-at-point            ; makes completion-at-point use ido
   ido-ubiquitous          ; use ido everywhere
   smex                    ; better M-x
   expand-region           ; easily expand/contract selections
   undo-tree               ; better undo/redo
   smart-forward           ; better jumping forward/backwards
   dired-details           ; more details to dired
   multiple-cursors        ; ability to use a lot of cursors
   diminish                ; unclutter the mode line
   magit                   ; magical git interface for emacs
   move-text               ; easily move lines up/down
   fill-column-indicator   ; line to indicate the fill column
   ))

;; =======================================
;; ======================== Setup packages
;; =======================================

;; Use the zenburn theme
(load-theme 'zenburn)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))

;; Load packages
(require 'use-package)

(use-package expand-region)
(use-package change-inner)
(use-package smart-forward)

(use-package setup-hippie)

(use-package fill-column-indicator
  :init
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "dark gray")
    (setq fci-rule-column 80)
    (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
    (global-fci-mode 1)))

(use-package smartparens-config
  :diminish smartparens-mode
  :init
  (progn
    (setq sp-autoescape-string-quote nil)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)))

(use-package smex
  :init
  (progn
    (smex-initialize)
    (setq smex-save-file (concat data-dir ".smex-items"))))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :init (global-git-gutter+-mode t))

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode))

(use-package multiple-cursors
  :init (setq mc/list-file (concat data-dir ".mc-lists")))

(use-package undo-tree
  :init (setq global-undo-tree-mode t))

(use-package browse-kill-ring
  :init (setq browse-kill-ring-quit-action 'save-and-restore))

;; Default to compiling latex files to pdf
(setq TeX-PDF-mode t)
;; Auto save when compiling
(setq TeX-save-query nil)

;; load all files in defuns-dir
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load custom keybindings
(require 'key-bindings)

(provide  'init)

;; =======================================
;; ====================== Interface tweaks
;; =======================================

;; Hide the splash screen
(setq inhibit-startup-message t)

;; Turn off excess interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use Source Code Pro font if it is available
(if (null (x-list-fonts "Source Code Pro Semibold"))
    nil 
  (set-face-attribute 'default nil :font "Source Code Pro Semibold"))

;; =======================================
;; ===================== Setup directories
;; =======================================

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

(require 'package)

;; Add melpa and marmalade as a package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

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
   markdown-mode           ; minor mode for markdown
   sml-mode                ; major mode for editing Standard ML
;; Themes
   zenburn-theme           ; the great zenburn theme
;; Packages
   visual-regexp-steroids  ; regex builder
   flx                     ; flexible matching
   flx-ido                 ; flexible matching in ido mode
   change-inner            ; easily change the inner or outer content of something
   ido-at-point            ; makes completion-at-point use ido
   ido-ubiquitous          ; use ido everywhere
   smex                    ; better M-x
   expand-region           ; easily expand/contract selections
   undo-tree               ; better undo/redo
   smart-forward           ; better jumping forward/backwards
   dired-details           ; more details to dired
   multiple-cursors        ; ability to use a lot of cursors
   yasnippet               ; snippet expansion
   diminish                ; unclutter the mode line
   magit                   ; magical git interface for emacs
   magit-log-edit          ; use the old commit log in magit
   move-text               ; easily move lines up/down
))

;; =======================================
;; ====================== Tweaks and hacks
;; =======================================

;; Use the zenburn theme

(load-theme 'zenburn)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'magit '(require 'magit-log-edit))

;; Clean up the mode-line
(eval-after-load 'yasnippet '(diminish 'yas-minor-mode))
(eval-after-load 'smartparens '(diminish 'smartparens-mode))
(eval-after-load 'git-gutter+ '(diminish 'git-gutter+-mode))

(require 'yasnippet)
(require 'setup-yasnippet)

(require 'setup-hippie)

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; Language specific setup files
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Map files to modes
(require 'mode-mappings)

;; load packages
(require 'expand-region)
(require 'change-inner)
(require 'multiple-cursors)
(require 'smart-forward)
(require 'git-gutter+)
(global-git-gutter+-mode t)
(require 'smex)
(smex-initialize)
(require 'undo-tree)
(setq global-undo-tree-mode t)
(require 'flycheck)
(global-flycheck-mode)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load custom keybindings
(require 'key-bindings)

(provide  'init)
;;; init.el ends here

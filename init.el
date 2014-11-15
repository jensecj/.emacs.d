;; =======================================
;; ====================== Interface tweaks
;; =======================================

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

;; Define our directories
(defconst root-dir "~/.emacs.d/")
(defconst lisp-dir (concat root-dir "lisp/")) ; home brewed lisp things
(defconst data-dir (concat root-dir "data/")) ; config data
(defconst backup-dir (concat data-dir "backups/")) ; backups and auto saves
(defconst modes-dir (concat root-dir "modes/")) ; home brewed modes

(unless (file-exists-p lisp-dir)
  (make-directory lisp-dir))

(unless (file-exists-p data-dir)
  (make-directory data-dir))

(unless (file-exists-p backup-dir)
  (make-directory backup-dir))

(unless (file-exists-p modes-dir)
  (make-directory modes-dir))

;; Add .emacs.d to load-path
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path modes-dir)

;; Keep emacs custom settings in a separate file
(setq custom-file (concat root-dir "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;; Save backup, auto save and temp files in data folder
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
(setq auto-save-list-file-prefix backup-dir)
(setq temporary-file-directory data-dir)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(require 'tramp)
(setq tramp-persistency-file-name (concat data-dir ".tramp"))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat data-dir ".places"))

;; =======================================
;; ====================== Install packages
;; =======================================

(require 'package)

;; Add melpa as a package archive
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defun file-age (file)
  (float-time
   (time-subtract (current-time)
                  (nth 5 (file-attributes (file-truename file))))))

;; Refresh package archive if it does not exist or is older than a week
(defconst melpa-archive (concat root-dir "elpa/archives/melpa"))

(unless (file-exists-p melpa-archive)
  (package-refresh-contents))

(if (> (file-age melpa-archive) 604800)
    (package-refresh-contents))

;; Install missing packages
(defun install-packages (packages)
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
        packages))

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
   ace-jump-buffer         ; jump between buffers
   ace-jump-zap            ; zap-to-char in ace-jump style
   smartparens             ; automatically add end parens, highlight matching parens
   git-gutter+             ; mark added/changes/removed lines in the gutter
   flycheck                ; linting
   smooth-scrolling        ; add a top and bottom margin when scrolling
   powerline               ; vim-esque powerline
   yasnippet               ; more auto-completion
   auto-complete           ; auto completion
   auto-complete-auctex    ; auto-completion for auctex
   auto-complete-clang     ; auto-completion for clang
   auto-complete-c-headers ; auto-completion for c/c++ header files
   god-mode                ; mode that prefixes C-
   ;; Major modes
   markdown-mode           ; markdown editing
   lua-mode                ; lua editing
   ;; Themes
   zenburn-theme           ; the great zenburn theme
   ;; Misc
   auctex                  ; latex editing
   use-package             ; pretty package initialization
   visual-regexp-steroids  ; better regular expressions
   browse-kill-ring        ; browse the kill ring
   change-inner            ; easily change the inner or outer content of something
   flx                     ; flexible matching
   flx-ido                 ; flexible matching in ido mode
   ido-at-point            ; makes completion-at-point use ido
   ido-ubiquitous          ; use ido everywhere
   smex                    ; better M-x
   expand-region           ; easily expand/contract selections
   undo-tree               ; better undo/redo
   dired+                  ; more dired features
   multiple-cursors        ; ability to use a lot of cursors
   diminish                ; unclutter the mode line
   magit                   ; magical git interface for emacs
   move-text               ; easily move lines up/down
   fill-column-indicator   ; line to indicate the fill column
   goto-chg                ; go to last change
   ag                      ; speedy search
   git-timemachine         ; easily check file changes through commits
   ))

;; =======================================
;; ======================== Setup packages
;; =======================================

;; Use the zenburn theme
(load-theme 'zenburn)

;; Lets start with some better defaults
(require 'setup-defaults)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'magit '(require 'setup-magit))
(require 'setup-hippie)
(require 'setup-undo-tree)
(require 'setup-powerline)
(require 'setup-autocomplete)

;; Setup local modes
(require 'setup-modes)

;; Load packages
(require 'setup-packages)

;; load all files in defuns-dir
(setq defuns-dir (concat root-dir "defuns/"))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'notes)

;; Load custom key bindings
(require 'key-bindings)

(provide 'init)

(require 'package)

;; Add melpa as a package archive
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

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
   workgroups2             ; workgroups in emacs
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

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'magit '(require 'setup-magit))
(require 'setup-hippie)
(require 'setup-undo-tree)
(require 'setup-powerline)
(require 'setup-autocomplete)

;; Load packages
(require 'setup-packages)

(provide 'init-packages)

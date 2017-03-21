(require 'package)

;; Add melpa as a package archive
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Refresh package archive if it does not exist or is older than a week
(defconst melpa-archive (concat root-dir "elpa/archives/melpa"))

(if (or (not (file-exists-p melpa-archive)) ;; if there exists no archive cache
        (> (file-age melpa-archive) 604800)) ;; or if the cache is old (a week = 60s * 60m * 24h * 7d)
    (package-refresh-contents)) ;; update the package archive cache

(defun package-installed-and-up-to-date-p (package)
  (when (package-installed-p package)
    (let* ((newest-desc (cadr (assq package package-archive-contents)))
           (installed-desc (cadr (or (assq package package-alist)
                                     (assq package package--builtins))))
           (newest-version  (package-desc-version newest-desc))
           (installed-version (package-desc-version installed-desc)))
      (version-list-<= newest-version installed-version))))

(defun install-packages (packages)
  "Install a list of packages, skip packages that are already installed."
  (mapc (lambda (package)
          (unless (package-installed-and-up-to-date-p package)
            (package-install package)))
        packages))

(require 'cl)
(defun is-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))

;; Install missing packages
(when (is-online?)
  (install-packages
   '(
     ;; Libraries
     s                       ; string manipulation library
     dash                    ; list library
     ;; Minor modes
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
     ;; Major modes
     markdown-mode           ; markdown editing
     lua-mode                ; lua editing
     tuareg                  ; ocaml
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
     git-timemachine         ; easily check file changes through commits
     wgrep                   ; editable grep buffer
     el-get                  ; more package management
     multi-term              ; manage multiple terminals
     jist                    ; manage github gists from emacs
     )
   )
  )

(defvar package-init-files
  '(
    init-package-ido
    init-package-magit
    init-package-hippie
    init-package-undo-tree
    init-package-powerline
    init-package-autocomplete
    init-package-use-packages
    ))

;; Safely load all the init files
(message "## Started loading package init files")
(dolist (file package-init-files)
  (safe-require file))
(message "## Finished loading package init files")

(add-to-list 'load-path (concat lisp-dir "misc/"))

(provide 'init-packages)
;;; init-packages.el ends here

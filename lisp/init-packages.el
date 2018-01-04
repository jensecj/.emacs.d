(require 'package)

;; Add melpa as a package archive
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(defconst melpa-archive (concat my-emacs-dir "elpa/archives/melpa"))

;; Refresh package archive if it does not exist or is older than a week
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
     ;; libraries
     dash                    ; list functions library (-map, -fold, etc.)
     s                       ; string manipulation library (concat, etc.)

     ;; file modes
     ;; auctex                  ; latex
     cmake-mode              ; CMake
     dockerfile-mode         ; dockerfiles
     gitconfig-mode          ; git config files
     gitignore-mode          ; git ignore files
     haskell-mode            ; haskell
     lua-mode                ; lua
     markdown-mode           ; markdown
     rust-mode               ; rust
     scss-mode               ; sassy css
     tuareg                  ; ocaml
     yaml-mode               ; yaml/yml

     ;; programming language specific
     ;; ac-c-headers            ; auto-complete source for c/c++ header files
     ;; ac-clang                ; auto-complete source for clang
     ac-octave               ; auto-complete source for octave
     ac-rtags                ; auto-complate source for rtags
     auto-complete           ; the auto completion framework
     ;; auto-complete-auctex    ; auto-complete source for auctex
     chicken-scheme          ; extensions for scheme code
     clang-format            ; buffer cleanup using clang-format
     rtags                   ; tags for c++ using clang
     scheme-complete         ; auto-completion for scheme

     ;; project / workflow related
     counsel-projectile      ; rewritten projectile function using ivy
     persp-mode              ; group buffers together into perspectives (and their configurations)
     projectile              ; project based behaviours (based on .git/.svn/etc.)
     workgroups              ; required for persp-mode to save perspectives to file

     ;; general emacs things
     ace-jump-buffer         ; jump between buffers
     ace-jump-mode           ; jump around the buffer with ease
     ace-jump-zap            ; zap-to-char in ace-jump style
     beginend                ; better M-< and M-> (beginning/end of buffer jumps)
     browse-kill-ring        ; browse the kill ring
     change-inner            ; easily change the inner or outer content of something
     counsel                 ; functions rewritten with ivy
     delight                 ; change mode names in the mode-line, works with use-package
     diminish                ; unclutter the mode line (hide modes)
     dired+                  ; more dired features
     ;; el-get                  ; more package management, has the coq proofgeneral package
     exec-path-from-shell    ; grab env variables from outside emacs
     expand-region           ; easily expand/contract selections
     fill-column-indicator   ; line to indicate the fill column
     flx                     ; flexible(fuzzy) matching for completions
     flycheck                ; linting
     git-gutter+             ; mark added/changes/removed lines in the gutter
     git-timemachine         ; easily check file changes through commits
     goto-chg                ; go to last change
     ivy                     ; a new type of completion, ala ido
     jist                    ; manage github gists from emacs
     kurecolor               ; manipulate color strings by hue / contrast / brightness
     magit                   ; magical git interface for emacs
     move-text               ; easily move lines up/down
     multi-term              ; manage multiple terminals
     multiple-cursors        ; ability to use a lot of cursors
     powerline               ; vim-esque powerline
     rainbow-mode            ; color hex strings
     smartparens             ; automatically add end parens, highlight matching parens
     smex                    ; better M-x, counsel uses the data from this for ordering
     smooth-scrolling        ; add a top and bottom margin when scrolling
     swiper                  ; buffer isearch using ivy
     undo-tree               ; better undo/redo, also has a visualizer
     unicode-fonts           ; support all the unicode characters
     use-package             ; pretty package initialization
     visual-regexp-steroids  ; better regular expressions
     wgrep                   ; editable grep buffer
     yasnippet               ; insert templates based on snippets
     which-key               ; popup after 1s, showing keybindings based on prefix key pressed

     ;; themes
     zenburn-theme           ; the great zenburn theme
     )
   )
  )

(defvar package-init-files
  '(
    init-package-magit
    init-package-undo-tree
    init-package-powerline
    init-package-autocomplete
    init-package-use-packages
    ))

;; Safely load all the init files
(message "## \e[36m Started loading package init files \e[0m")
(dolist (file package-init-files)
  (try-require file))
(message "## \e[36m Finished loading package init files \e[0m")

(add-to-list 'load-path (concat my-emacs-lisp-dir "misc/"))

(provide 'init-packages)
;;; init-packages.el ends here

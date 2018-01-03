(require 'use-package)

(setq use-package-verbose t)

;; associate file names with modes
(use-package botcode-mode
  :mode "\\.bot\\'")
(use-package cmake-mode
  :mode "\\CmakeLists.txt\\'")
(use-package octave-mode
  :mode "\\.m\\'")
(use-package shell-script-mode
  :mode ("\\.sh\\'" "\\.zsh\\'" "\\zshrc\\'"))
(use-package scheme-mode ;; use chicken scheme
  :defer
  :config (setq scheme-program-name "csi -:c"))

(use-package latex
  :defer
  :config
  (setq-default TeX-PDF-mode t) ;; default to pdf
  (setq-default TeX-global-PDF-mode t) ;; default to pdf
  (setq-default TeX-parse-self t) ;; parse on load
  (setq-default TeX-auto-save t) ;; parse on save
  (setq-default TeX-save-query nil) ;; save before compiling
  (setq-default TeX-master nil) ;; try to figure out which file is the master
  (setq-default reftex-plug-into-AUCTeX t) ;; make reftex and auctex work together
  (add-hook 'LaTeX-mode-hook 'reftex-mode) ;; enable reftex
  )

(use-package auto-complete
  :diminish auto-complete-mode
  :demand
  :bind
  (("C-+" . ac-quick-help-at-point)
   ("C-<tab>" . auto-complete))
  :config
  (setq ac-auto-start t) ;; auto start completing
  (setq ac-show-menu t) ;; show the menu instantly
  (setq ac-show-menu-immediately-on-auto-complete t) ;; show the autocompletion menu instantly
  (setq ac-delay 0.1) ;; show completion menu quickly
  (setq ac-use-quick-help t) ;; use the help
  (setq ac-quick-help-delay 0.1) ;; show help quickly
  (setq ac-use-comphist t)
  (setq ac-comphist-file "~/.emacs.d/data/.ac-comphist") ;; move the history file
  (setq ac-ignore-case t)
  (setq-default ac-sources
                '(ac-source-imenu
                  ac-source-words-in-same-mode-buffers))
  ;; '(ac-source-words-in-buffer ac-source-imenu ac-source-yasnippet)
  (global-auto-complete-mode t))

(use-package smex
  :config
  (smex-initialize)
  (setq smex-save-file (concat data-dir ".smex-items")))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-autoescape-string-quote nil)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config (global-git-gutter+-mode t))

(use-package multiple-cursors
  :bind
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-all-like-this)
   ("C-M-a" . set-rectangular-region-anchor))
  :config
  (setq mc/list-file (concat data-dir ".mc-lists")))

(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package ace-jump-mode
  :bind
  (("C-ø" . ace-jump-char-mode)
   ("C-'" . ace-jump-line-mode)))

(use-package ace-jump-buffer
  :bind ("C-x C-b" . ace-jump-buffer))

(use-package ace-jump-zap
  :bind ("C-å" . ace-jump-zap-to-char))

(use-package expand-region
  :bind
  (("M-e" . er/expand-region)
   ("C-M-e" . er/contract-region)))

(use-package change-inner
  :bind
  (("M-i" . copy-inner)
   ("M-o" . copy-outer)
   ("M-I" . change-inner)
   ("M-O" . change-outer)))

(use-package move-text
  :bind
  (("C-S-<up>" . move-text-up)
   ("C-S-<down>" . move-text-down)))

(use-package visual-regexp-steroids
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

(use-package counsel
  :diminish counsel-mode
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("C-S-s" . counsel-rg)
   ("C-x f" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-i" . counsel-imenu)
   ("M-x" . counsel-M-x)
   ("M-b" . counsel-bookmark))
  :config
  (setq
   counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-mode))

(use-package ivy
  :demand
  :bind
  (:map ivy-minibuffer-map
        ("C-d" . (lambda () (interactive) (ivy-quit-and-run (dired ivy--directory))))
        ("C-S-<return>" . ivy-immediate-done))
  :diminish ivy-mode
  :config
  (setq ivy-height 15)
  (setq ivy-count-format "")
  (ivy-mode))

(use-package counsel-projectile)
(use-package projectile
  :after counsel-projectile
  :diminish projectile-mode
  :config (counsel-projectile-mode))

(use-package rtags
  :diminish rtags-mode
  :bind
  (:map c++-mode-map
        ("M-." . rtags-find-symbol-at-point)
        ("M-," . rtags-location-stack-back)))

(use-package magit
  :bind
  (("C-x m" . magit-status)
   :map magit-mode-map
   ("C-c C-a" . magit-commit-amend)
   ("q" . magit-quit-session))
  :config
  (setq magit-auto-revert-mode nil))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (("C-x u" . undo-tree-visualize)
   ("C-_" . undo-tree-undo)
   ("M-_" . undo-tree-redo))
  :config
  (global-undo-tree-mode))

(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 5)
  (smooth-scrolling-mode))

(use-package goto-chg
  :bind ("M-ø" . goto-last-change))

(use-package dired+)
(use-package dired
  :after dired+
  :bind
  (("C-x C-d" . (lambda () (interactive) (dired default-directory)))
   :map dired-mode-map
   ("C-c C-." . dired-dotfiles-toggle)
   ("<backspace>" . diredp-up-directory-reuse-dir-buffer))
  :config
  (toggle-diredp-find-file-reuse-dir 1)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 60 -1 :left) " "
                (filename-and-process 70 -1))
          (mark " " (name 16 -1) " " filename))))

(use-package subword
  :diminish subword-mode)

(use-package wgrep
  :init (require 'grep)
  :bind
  (("C-S-g" . rgrep)
   :map grep-mode-map
   ("C-x C-q" . wgrep-change-to-wgrep-mode)
   ("C-x Ck" . wgrep-abort-changes)
   ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t))

(use-package multi-term
  :bind ("C-z" . better-multi-term)
  :config
  (defun better-multi-term ()
    "Create new term buffer."
    (interactive)
    (let ((term-buffer)
          (buffer-new-name (file-name-directory buffer-file-name)))
      ;; Set buffer.
      (setq term-buffer (multi-term-get-buffer current-prefix-arg))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (switch-to-buffer term-buffer)
      (rename-buffer (concat "*" buffer-new-name "*"))))
  (setq multi-term-program "/bin/zsh")
  (setq term-bind-key-alist '()) ;; clear the binds list, defaulting to emacs binds
  )

(use-package jist
  :config (setq jist-enable-default-authorized 't))

(use-package unicode-fonts
  :config (unicode-fonts-setup))

(use-package exec-path-from-shell
  :config
  ;; try to grab the ssh-agent if it is running
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package beginend
  :init
  (global-set-key (kbd "M-<") 'beginning-of-buffer)
  (global-set-key (kbd "M->") 'end-of-buffer)
  :config
  ;; diminish all the beginend modes
  (mapc (lambda (s) (diminish (cdr s))) beginend-modes)
  (beginend-global-mode))

;; (use-package persp-mode
;;   :config
;;   (persp-set-keymap-prefix (kbd "C-M-p"))
;;   (setq wg-morph-on nil)
;;   (setq persp-auto-resume-time -1)
;;   (persp-mode 1)
;;   )

;; (use-package fill-column-indicator
;;   :diminish fci-mode
;;   :config
;;   (setq fci-rule-width 1)
;;   (setq fci-rule-color "grey")
;;   (setq fci-rule-column 80)
;;   ;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;   ;; (global-fci-mode 1)
;;   )

;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :bind
;;   (:map yas-minor-mode-map
;;         ("<tab>" . nil) ;; remove default expand keys
;;         ("TAB" . nil)
;;         ("M-<tab>" . yas-expand) ;; use M-tab instead
;;         ("<return>" . yas/exit-all-snippets)) ;; exit snippets on enter
;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   (yas-global-mode 1))

;; (use-package flycheck
;;   :diminish flycheck-mode
;;   :init (global-flycheck-mode)
;;   )

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  :custom-face
  (ivy-current-match ((t (:background "#4f4f4f" :weight bold :box t))))
  (diredp-dir-priv ((t (:foreground "#8CD0D3"))))
  (diredp-file-name ((t (:foreground "#DCDCCC"))))
  (persp-face-lighter-buffer-not-in-persp ((t (:foreground "#CC9393"))))
  (ac-candidate-face ((t (:foreground "#F0DFAF" :background "#313131"))))
  (ac-selection-face ((t (:foreground "#FEFEFE" :background "#3E3E3E")))))

(provide 'init-package-use-packages)

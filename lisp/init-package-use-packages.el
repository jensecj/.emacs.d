(require 'use-package)

(use-package botcode-mode
  :mode "\\.bot\\'")

(use-package latex
  :defer t
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

(use-package fill-column-indicator
  :diminish fci-mode
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "grey")
  (setq fci-rule-column 80)
  ;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
  ;; (global-fci-mode 1)
  )

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  (setq sp-autoescape-string-quote nil)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config (global-git-gutter+-mode t))

;; (use-package flycheck
;;   :diminish flycheck-mode
;;   :init (global-flycheck-mode)
;;   )

(use-package multiple-cursors
  :bind
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-all-like-this)
   ("C-M-a" . set-rectangular-region-anchor))
  :config
  (setq mc/list-file (concat data-dir ".mc-lists")))

(use-package browse-kill-ring
  :config
  (setq browse-kill-ring-quit-action 'save-and-restore))

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
   ("M-x" . counsel-M-x)
   ("M-b" . counsel-bookmark))
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-height 15)
  (setq ivy-count-format ""))

(use-package counsel-projectile)
(use-package projectile
  :diminish projectile-mode
  :config
  (counsel-projectile-on))

(use-package rtags
  :diminish rtags-mode
  :bind
  (("M-ø" . rtags-find-symbol-at-point)
   ("M-æ" . rtags-location-stack-back)))

(use-package magit
  :bind ("C-x m" . magit-status)
  :config
  (define-key magit-mode-map (kbd "C-c C-a") 'magit-commit-amend)
  (define-key magit-mode-map (kbd "q") 'magit-quit-session)
  (setq magit-auto-revert-mode nil))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind (("C-x u" . undo-tree-visualize)
         ("C-_" . undo-tree-undo)
         ("M-_" . undo-tree-redo))
  :config (global-undo-tree-mode))

(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring))

(use-package smooth-scrolling)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (define-key yas-minor-mode-map (kbd "<tab>") nil) ;; remove default expand key
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-<tab>") 'yas-expand) ;; use M-tab instead
  (define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets) ;; exit snippets on enter
  (yas-global-mode 1))

(use-package goto-chg
  :bind ("M-." . goto-last-change))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   `(ivy-current-match ((t (:background "#4f4f4f" :weight bold :box t))))
   `(diredp-dir-priv ((t (:foreground "#8CD0D3"))))
   `(diredp-file-name ((t (:foreground "#DCDCCC"))))))

(use-package dired+
  :bind (("C-x C-d" . dired)
         ("C-c C-." . dired-dotfiles-toggle))
  :config
  (require 'dired)
  (define-key dired-mode-map (kbd "<backspace>") 'diredp-up-directory-reuse-dir-buffer)
  (toggle-diredp-find-file-reuse-dir 1)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 60 -1 :left) " "
                (filename-and-process 70 -1))
          (mark " " (name 16 -1) " " filename))))

(use-package subword
  :diminish subword-mode)

(use-package wgrep
  :bind ("C-S-g" . rgrep)
  :config
  (require 'grep)
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "C-x C-k") 'wgrep-abort-changes)
  (define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit)
  (setq wgrep-auto-save-buffer t))

(use-package multi-term
  :bind ("C-z" . better-multi-term)
  :config
  (require 'multi-term)
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
  :config
  (setq jist-enable-default-authorized 't))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package exec-path-from-shell
  :init
  ;; try to grab the ssh-agent if it is running
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))


(provide 'init-package-use-packages)

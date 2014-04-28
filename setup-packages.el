(require 'use-package)

;; (use-package fill-column-indicator
;;   :init
;;   (progn
;;     (setq fci-rule-width 1)
;;     (setq fci-rule-color "dark gray")
;;     (setq fci-rule-column 80)
;;     (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;     (global-fci-mode 1)))

(use-package smartparens-config
  :diminish smartparens-mode
  :init
  (progn
    (setq sp-autoescape-string-quote nil)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
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
  :bind
  (("C-d" . mc/mark-next-like-this)
   ("C-S-d" . mc/mark-all-like-this)
   ("C-M-a" . set-rectangular-region-anchor))
  :config
  (setq mc/list-file (concat data-dir ".mc-lists")))

(use-package undo-tree
  :init (setq global-undo-tree-mode t))

(use-package browse-kill-ring
  :init (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package ace-jump-mode
  :bind
  (("C-ø" . ace-jump-mode)
   ("C-Ø" . ace-jump-char-mode)
   ("C-'" . ace-jump-line-mode)))

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

(use-package smart-forward
  :bind
  (("M-<up>" . smart-up)
   ("M-<down>" . smart-down)
   ("M-<left>" . smart-backward)
   ("M-<right>" . smart-forward)))

(use-package move-text
  :bind
  (("C-S-<up>" . move-text-up)
   ("C-S-<down>" . move-text-down)))

(use-package visual-regexp-steroids
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-s" . vr/isearch-forward)
   ("C-r" . vr/isearch-backward)
   ("C-M-s" . vr/mc-mark)))

(use-package magit
  :bind ("C-x m" . magit-status)
  :config (setq magit-auto-revert-mode nil))

(use-package undo-tree
  :bind
  (("C-x u" . undo-tree-visualize)
   ("C-_" . undo-tree-undo)
   ("M-_" . undo-tree-redo)))

(use-package browse-kill-ring
  :bind
  (("C-x C-y" . browse-kill-ring)))

(use-package ace-jump-buffer
  :bind ("C-x C-b" . ace-jump-buffer))

(use-package smooth-scrolling)

(provide 'setup-packages)

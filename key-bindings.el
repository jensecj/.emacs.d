;; =======================================
;; ============================== Packages
;; =======================================

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'ace-jump-mode)
(global-set-key (kbd "C-ø") 'ace-jump-mode)
(global-set-key (kbd "C-Ø") 'ace-jump-char-mode)
(global-set-key (kbd "C-'") 'ace-jump-line-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-a") 'set-rectangular-region-anchor)

(require 'expand-region)
(global-set-key (kbd "M-e") 'er/expand-region)
(global-set-key (kbd "C-M-e") 'er/contract-region)

(require 'change-inner)
(global-set-key (kbd "M-i") 'copy-inner)
(global-set-key (kbd "M-o") 'copy-outer)
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

(require 'smart-forward)
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

(require 'move-text)
(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)

(require 'visual-regexp)
(global-set-key (kbd "M-&") 'vr/query-replace)
(global-set-key (kbd "M-/") 'vr/replace)

(require 'magit)
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

(require 'undo-tree)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "M-_") 'undo-tree-redo)

;; =======================================
;; ========== Built-in features and Defuns
;; =======================================

;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Fix spaces / tabs
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Evaluate the current buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Evaluate the current region
(global-set-key (kbd "C-c k") 'eval-region)

;; Join lines upward
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Jump to symbol definitions
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Move windows with S-<arrow>
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;; Force save a file
(global-set-key (kbd "C-x t") 'touch-buffer-file)

(provide 'key-bindings)

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

(autoload 'magit-status "magit")
(global-set-key (kbd "C-x m") 'magit-status)

(require 'undo-tree)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "M-_") 'undo-tree-redo)

(require 'browse-kill-ring)
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; =======================================
;; ===================== Built-in features
;; =======================================

;; Quit emacs, mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Evaluate the current buffer/region
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c k") 'eval-region)

;; Join lines
(global-set-key (kbd "M-j") ;; upwards
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "M-J") ;; downwards
                (lambda ()
                  (interactive)
                  (join-line)))

;; Scroll the buffer without moving the point (unless we over-move)
(global-set-key (kbd "C-<up>")
                (lambda ()
                  (interactive)
                  (scroll-down 3)))

(global-set-key (kbd "C-<down>")
                (lambda ()
                  (interactive)
                  (scroll-up 3)))

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Disable popups from the mouse
(global-set-key (kbd "C-<down-mouse-1>") nil)
(global-set-key (kbd "C-<down-mouse-3>") nil)

;; =======================================
;; ================================ Defuns
;; =======================================

;; Better C-a
(global-set-key (kbd "C-a") 'smart-line-beginning)

;; Jump to symbol definitions
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; Fix spaces / tabs
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
;; (global-set-key (kbd "M-t p") 'transpose-params) ;; TODO: make this better

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Move windows with S-<arrow>
(windmove-default-keybindings 'shift)

;; Force save a file, mnemonic is C-x TOUCH
(global-set-key (kbd "C-x t") 'touch-buffer-file)

;; Copy current line / region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

(global-set-key (kbd "C-x C-r") 'sudo-edit)

;; =======================================
;; ================================= Modes
;; =======================================

;; in c++ mode, compile using C-c C-c
(require 'cc-mode)
(add-hook 'c++-mode-hook '(lambda() (define-key c++-mode-map (kbd "C-c C-c") 'compile)))


(provide 'key-bindings)
;;; key-bindings.el ends here

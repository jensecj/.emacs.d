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

;; better copying on M-w
(global-set-key (kbd "M-w")
                (lambda ()
                  (interactive)
                  (save-excursion
                    (unless (use-region-p)
                      (beginning-of-line))
                    (save-region-or-current-line nil)
                    (message "copied"))))

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

;; Disable pop ups from the mouse
(global-set-key (kbd "C-<down-mouse-1>") nil)
(global-set-key (kbd "C-<down-mouse-3>") nil)

;; Jump to symbol definitions
(global-set-key (kbd "C-x C-i") 'imenu)

;; Move the delete windows, mnemonic is C-x OTHER
(global-set-key (kbd "C-x 0") nil)
(global-set-key (kbd "C-x 1") nil)
(global-set-key (kbd "C-x o") 'delete-other-windows)
(global-set-key (kbd "C-x p") 'delete-window)

;; Make Home and End to to the top and bottom of the buffer, we have C-a/e
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; =======================================
;; ================================ Defuns
;; =======================================

;; Better C-a
(global-set-key (kbd "C-a") 'smart-line-beginning)

;; Fix spaces / tabs
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Enable backwards killing of lines
(global-set-key (kbd "C-S-k") 'kill-to-beginning-of-line)

;; Toggle window split
(global-set-key (kbd "C-<tab>") 'toggle-window-split)
(global-set-key (kbd "M-<tab>") 'rotate-windows)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
;; (global-set-key (kbd "M-t p") 'transpose-params) ;; TODO: make this better

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x b") 'ibuffer)

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

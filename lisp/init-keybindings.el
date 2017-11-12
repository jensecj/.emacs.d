(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; =======================================
;; ===================== Built-in features
;; =======================================

;; Insert tilde with a single keystroke
(define-key my-keys-minor-mode-map (kbd "<menu>") (lambda () (interactive) (insert "~")))

;; Easily mark the entire buffer
(define-key my-keys-minor-mode-map (kbd "C-x a") 'mark-whole-buffer)

;; Quit emacs, mnemonic is C-x REALLY QUIT
(define-key my-keys-minor-mode-map (kbd "C-x r q") 'save-buffers-kill-terminal)
;; Kill emacs, mnemonic is C-x REALLY KILL
(define-key my-keys-minor-mode-map (kbd "C-x r k") 'save-buffers-kill-emacs)

;; (define-key my-keys-minor-mode-map (kbd "C-x C-c") '())

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key my-keys-minor-mode-map (kbd "<f1>") 'help-command)

;; Evaluate the current buffer/region
(define-key my-keys-minor-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c k") 'eval-region)

;; Join lines
(define-key my-keys-minor-mode-map (kbd "M-j") ;; downwards
  (lambda ()
    (interactive)
    (if (region-active-p)
        (progn
          (join-region))
      (join-line -1))))

;; Scroll the buffer without moving the point (unless we over-move)
(define-key my-keys-minor-mode-map (kbd "C-<up>")
  (lambda ()
    (interactive)
    (scroll-down 3)))

(define-key my-keys-minor-mode-map (kbd "C-<down>")
  (lambda ()
    (interactive)
    (scroll-up 3)))

;; Comment/uncomment block
(define-key my-keys-minor-mode-map (kbd "C-c c")
  (lambda ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

(define-key my-keys-minor-mode-map (kbd "C-c u") 'uncomment-region)

;; Disable pop ups from the mouse
(define-key my-keys-minor-mode-map (kbd "C-<down-mouse-1>") nil)
(define-key my-keys-minor-mode-map (kbd "C-<down-mouse-3>") nil)
(define-key my-keys-minor-mode-map (kbd "S-<down-mouse-1>") nil)

;; Disable suspend-frame
(global-set-key "\C-x\C-z" nil)

;; Jump to symbol definitions
(define-key my-keys-minor-mode-map (kbd "C-x C-i") 'imenu)

;; Move the delete windows, mnemonic is C-x OTHER
(define-key my-keys-minor-mode-map (kbd "C-x 0") nil)
(define-key my-keys-minor-mode-map (kbd "C-x 1") nil)
(define-key my-keys-minor-mode-map (kbd "C-x o") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-x p") 'delete-window)

;; Make Home and End to to the top and bottom of the buffer, we have C-a/e
(define-key my-keys-minor-mode-map (kbd "<home>") 'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "<end>") 'end-of-buffer)

(define-key my-keys-minor-mode-map (kbd "M-<left>") 'backward-sexp)
(define-key my-keys-minor-mode-map (kbd "M-<right>") 'forward-sexp)

;; =======================================
;; ================================ Defuns
;; =======================================

;; Better C-a
(define-key my-keys-minor-mode-map (kbd "C-a") 'smart-line-beginning)

;; Fix spaces / tabs
(define-key my-keys-minor-mode-map (kbd "C-c n") 'cleanup-buffer)

;; Enable backwards killing of lines
(define-key my-keys-minor-mode-map (kbd "C-S-k") 'kill-to-beginning-of-line)

;; Toggle window split
(define-key my-keys-minor-mode-map (kbd "M-C-<tab>") 'toggle-window-split)
(define-key my-keys-minor-mode-map (kbd "M-S-<iso-lefttab>") 'rotate-windows)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(define-key my-keys-minor-mode-map (kbd "M-t w") 'transpose-words)
(define-key my-keys-minor-mode-map (kbd "M-t s") 'transpose-sexps)
;; (define-key my-keys-minor-mode-map (kbd "M-t p") 'transpose-params) ;; TODO: make this better

(define-key my-keys-minor-mode-map (kbd "C-x b") 'ibuffer)

;; Move windows with S-<arrow>
(windmove-default-keybindings 'shift)

;; Force save a file, mnemonic is C-x TOUCH
(define-key my-keys-minor-mode-map (kbd "C-x t") 'touch-buffer-file)

;; Copy current line / region
(define-key my-keys-minor-mode-map (kbd "M-w") 'save-region-or-current-line)
(define-key my-keys-minor-mode-map (kbd "C-w") 'kill-region-or-current-line)

;; Completion that uses many different methods to find options.
(define-key my-keys-minor-mode-map (kbd "C-.") 'hippie-expand-no-case-fold)
(define-key my-keys-minor-mode-map (kbd "C-:") 'hippie-expand-lines)
(define-key my-keys-minor-mode-map (kbd "C-,") 'completion-at-point)

(define-key my-keys-minor-mode-map (kbd "C-x C-r") 'sudo-edit)

;; keybindings for window resizing
(define-key my-keys-minor-mode-map (kbd "M-S-<left>") 'move-border-left)
(define-key my-keys-minor-mode-map (kbd "M-S-<right>") 'move-border-right)
(define-key my-keys-minor-mode-map (kbd "M-S-<up>") 'move-border-up)
(define-key my-keys-minor-mode-map (kbd "M-S-<down>") 'move-border-down)

(define-key my-keys-minor-mode-map (kbd "M-g M-g") 'goto-line-with-feedback)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t nil 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(provide 'init-keybindings)

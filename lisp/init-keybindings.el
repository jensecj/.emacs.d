;; =======================================
;; ===================== Built-in features
;; =======================================

;; handle special keys
(define-key key-translation-map [S-dead-circumflex] "^")
(define-key key-translation-map [dead-tilde] "~")
(define-key key-translation-map [S-dead-grave] "´")
(define-key key-translation-map [dead-acute] "`")

;; Insert tilde with a single keystroke
(global-set-key (kbd "<menu>") (lambda () (interactive) (insert "~")))

;; Easily mark the entire buffer
(global-set-key (kbd "C-x a") 'mark-whole-buffer)

;; Quit emacs, mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
;; Kill emacs, mnemonic is C-x REALLY KILL
(global-set-key (kbd "C-x r k") 'save-buffers-kill-emacs)

;; don't close emacs
(global-set-key (kbd "C-x C-c") '())

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Evaluate the current buffer/region
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c k") 'eval-region)

;; Join lines (pull the below line up to this one)
(global-set-key
 (kbd "M-j")
 (lambda ()
   (interactive)
   (if (region-active-p)
       (progn
         (join-region))
     (join-line -1))))

;; Scroll the buffer without moving the point (unless we over-move)
(global-set-key
 (kbd "C-<up>")
 (lambda ()
   (interactive)
   (scroll-down 3)))

(global-set-key
 (kbd "C-<down>")
 (lambda ()
   (interactive)
   (scroll-up 3)))

;; Comment/uncomment block
(global-set-key
 (kbd "C-c c")
 (lambda ()
   (interactive)
   (if (region-active-p)
       (comment-or-uncomment-region (region-beginning) (region-end))
     (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

(global-set-key (kbd "C-c u") 'uncomment-region)

;; Disable pop ups from the mouse
(global-set-key (kbd "C-<down-mouse-1>") nil)
(global-set-key (kbd "C-<down-mouse-3>") nil)
(global-set-key (kbd "S-<down-mouse-1>") nil)

;; Disable suspend-frame
(global-set-key "\C-x\C-z" nil)

;; Move the delete windows, mnemonic is C-x OTHER
(global-set-key (kbd "C-x 0") nil)
(global-set-key (kbd "C-x 1") nil)
(global-set-key (kbd "C-x o") 'delete-other-windows)
(global-set-key (kbd "C-x p") 'delete-window)

;; Make Home and End to to the top and bottom of the buffer, we have C-a/e
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "M-<right>") 'forward-sexp)

;; find things at point
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "C-M-.") 'xref-find-definitions-other-window)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)

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
(global-set-key (kbd "M-C-<tab>") 'toggle-window-split)
(global-set-key (kbd "M-S-<iso-lefttab>") 'rotate-windows)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
;; (global-set-key (kbd "M-t p") 'transpose-params) ;; TODO: make this better

(global-set-key (kbd "C-x b") 'ibuffer)

;; Move windows with S-<arrow>
(windmove-default-keybindings 'shift)

;; Force save a file, mnemonic is C-x TOUCH
(global-set-key (kbd "C-x t") 'touch-buffer-file)

;; Copy current line / region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "C-w") 'kill-region-or-current-line)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

;; keybindings for window resizing
(global-set-key (kbd "M-S-<left>") 'move-border-left)
(global-set-key (kbd "M-S-<right>") 'move-border-right)
(global-set-key (kbd "M-S-<up>") 'move-border-up)
(global-set-key (kbd "M-S-<down>") 'move-border-down)

(global-set-key (kbd "M-g M-g") 'goto-line-with-feedback)

(provide 'init-keybindings)

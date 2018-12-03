;;; experimental.el --- Lisp experiments

(defun download-to-current-buffer-directory (link)
  "Download LINK into the working directory for the current buffer."
  (interactive)
  (letrec ((-silence-output " -so - ")
           (-use-user-agent "'-A User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36'")
           (filename (f-filename link))
           (curl-command (concat "curl" " " -use-user-agent " '" link "'" -silence-output " > " filename)))
    (s-trim (shell-command-to-string curl-command))))

(use-package evil
  :ensure t
  :disabled t)

(defun modal-movement ()
  (interactive)
  (message "MODAL MOVEMENT MODE")
  (jens/one-shot-keymap
   '(("i" . (lambda () (interactive) (previous-line)))
     ("k" . (lambda () (interactive) (next-line)))
     ("j" . (lambda () (interactive) (backward-char)))
     ("l" . (lambda () (interactive) (forward-char)))

     ("C-i" . (lambda () (interactive) (scroll-down 5)))
     ("C-k" . (lambda () (interactive) (scroll-up 5)))
     ("C-j" . (lambda () (interactive) (left-word)))
     ("C-l" . (lambda () (interactive) (right-word)))

     ("w" . (lambda (arg) (interactive "P") (jens/kill-region-or-current-line arg)))
     ("e" . (lambda () (interactive) (end-of-line)))
     ("a" . (lambda () (interactive) (jens/smart-beginning-of-line)))
     ("u" . (lambda () (interactive) (scroll-down-command)))
     ("o" . (lambda () (interactive) (scroll-up-command)))
     ))
  )

;; (global-set-key (kbd "<escape>") 'modal-movement)

(provide 'experimental)

;;; etmux.el --- Communicating with tmux from emacs

(defun etmux-tmux-running-p ()
  "Return whether `tmux' is running on the system."
  (zerop (process-file "tmux" nil nil nil "has-session")))

(defun etmux-tmux-run-command (&rest args)
  "Run a tmux-command in the running tmux session."
  (with-temp-buffer
    (let ((retval (apply 'process-file "tmux" nil (current-buffer) nil args)))
      (if (zerop retval)
          (buffer-string)
        (error (format "Failed: %s(status = %d)" (mapconcat 'identity (cons "tmux" args) " ") retval))))))

(defun etmux--send-keys (target keys)
  "Send a key combination to the tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target keys "C-m"))

(defun etmux-reset-prompt (target)
  "Clears the prompt of the tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target "C-u"))

(defun etmux-clear (target)
  "Clears the screen of the tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target "C-l"))

(defun etmux-C-c (target)
  "Send interrupt signal to tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target "C-c"))

(defun etmux-C-d (target)
  "Send EOF signal to tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target "C-d"))

(defun etmux-C-z (target)
  "Send TSTP signal to tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target "C-z"))

;;;###autoload
(defun etmux-run-command (target command)
  "Send a command to the tmux target."
  (interactive)
  (when (etmux-tmux-running-p)
    (etmux--reset-prompt target)
    (etmux--send-keys target command)))

(provide 'etmux)
;;; etmux.el ends here

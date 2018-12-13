;;; etmux.el --- Communicating with tmux from emacs

(defun etmux-tmux-running-p ()
  "Return whether `tmux' is running on the system."
  (zerop (process-file "tmux" nil nil nil "has-session")))

(defun etmux-tmux-run-command (&rest args)
  "Run a tmux-command in the running tmux session."
  (let ((retval (apply 'process-file "tmux" nil nil nil args)))
    (unless (zerop retval)
      (error (format "Failed: %s(status = %d)" (mapconcat 'identity (cons "tmux" args) " ") retval)))))

(defun etmux--reset-prompt (target)
  "Clears the prompt of the tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target "C-u"))

(defun etmux--clear-screen (target)
  "Clears the screen of the tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target "C-l"))

;;;###autoload
(defun etmux-send-keys (target input)
  "Send a key combination to the tmux target."
  (etmux-tmux-run-command "send-keys" "-t" target input "C-m"))

;;;###autoload
(defun etmux-send-command (target command)
  "Send a command to the tmux target."
  (interactive)
  (when (etmux-tmux-running-p)
    (etmux--reset-prompt target)
    (etmux-send-keys target command)))

(provide 'etmux)
;;; etmux.el ends here

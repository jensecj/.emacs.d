;; Lisp specific defuns

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defmacro with-supressed-message (&rest body)
  "Saves the current message in the minibuffer, executes body, then
restores the message."
  (let ((saved-message-symbol (make-symbol "saved-message")))
    `(let ((,saved-message-symbol (current-message)))
       (progn ,@body)
       (message ,saved-message-symbol))))

;; a place for functions i wish were included in their respective packages

(defun counsel-read-file-name (prompt &optional initial-input)
  "Find a file path using ivy-read"
  (interactive)
  (ivy-read prompt 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action
            (lambda (x)
              (with-ivy-window
                (if (and counsel-find-file-speedup-remote
                         (file-remote-p ivy--directory))
                    (let ((find-file-hook nil))
                      (expand-file-name x ivy--directory))
                  (expand-file-name x ivy--directory))))
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (let ((f (ffap-guesser)))
                           (when f (expand-file-name f))))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-read-find-name))

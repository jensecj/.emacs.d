;;; package --- Summary
;;; Commentary:
;;; Code:
(defun notes-go ()
  "Go to the note file of the day."
  (interactive)
  (let ((date-file-path (concat "~/Dropbox/Notebooks/misc/" (concat (format-time-string "%Y-%m-%d") ".md"))))
    (find-file date-file-path)))

(provide 'notes)
;;; notes.el ends here

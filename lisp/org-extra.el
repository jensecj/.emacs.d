(defun org-subtree-remove-checkboxes (subtree &optional checkbox-state)
  "Returns SUBTREE with all checkboxes that are in CHECKBOX-STATE removed."
  (letrec ((checked-checkbox-regex "^\\- \\[X\\]")
           (empty-checkbox-regex "^\\- \\[ \\]")
           (checkbox-regex (if (eq checkbox-state 'checked)
                               checked-checkbox-regex
                             empty-checkbox-regex)))
    (with-temp-buffer
      (insert subtree)
      (goto-char (point-min))
      (org-mode)

      ;; walk through all the org-items, and remove the ones matching the
      ;; supplied checkbox regex
      (save-excursion
        (save-match-data
          (while (string-match checkbox-regex (buffer-string))
            (goto-char (match-end 0))

            (let ((beginning-of-item (save-excursion (org-beginning-of-item) (point)))
                  (end-of-item (save-excursion (org-end-of-item) (point))))
              (delete-region beginning-of-item end-of-item)))))

      ;; update the checkbox-count for the heading
      (org-update-checkbox-count 't)

      ;; if we remove all the incomplete checkboxes, this task becomes DONE.
      (when (not (eq checkbox-state 'checked))
        (org-todo))

      (buffer-string))))

(defun copy-subtree-at-point ()
  "Return a copy of the current subtree-at-point."
  (org-copy-subtree)
  (with-temp-buffer
    (yank)
    (buffer-string)))

(defun cut-subtree-at-point ()
  "Return the current subtree-at-point, cutting from the
document."
  (org-cut-subtree)
  (with-temp-buffer
    (yank)
    (buffer-string)))

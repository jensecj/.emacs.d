;;; -*- lexical-binding: t -*-

(require 'today-fs)
(require 'today-util)

(defun today-move--subtree-action (date)
  "Move the subtree-at-point, to the bottom of the file
corresponding to DATE."
  (let ((subtree (today-util-cut-subtree-at-point)))
    (save-buffer)
    (today-util-insert-entry subtree date)))

;;;###autoload
(defun today-move-to-tomorrow ()
  "Move the subtree-at-point to tomorrows file."
  (interactive)
  (letrec ((current-files-date (today-util-current-files-date))
           (tomorrows-date (today-util-date-add-days current-files-date 1)))
    (today-move--subtree-action tomorrows-date)))

;;;###autoload
(defun today-move-to-date (arg)
  "Move the subtree-at-point to a date selected with
`org-calendar', or, if using a prefix argument, move it n-days
relative to the current file. (also works with negative
prefixes)"
  (interactive "P")
  (letrec ((date (if arg
                     (today-util-date-add-days (today-util-current-files-date) arg)
                   (org-read-date))))
    (today-move--subtree-action date)))

(defcustom today-move--unfinished-task-regexp "^\\* TODO "
  "The regexp used to search for a incomplete tasks.")

(defun today-move--unfinished-to-date-action (source-date destination-date)
  "Move all unfinished tasks to DATEs file."
  ;; `today-move--subtree-action' is a destructive action, so each iteration
  ;; should have fewer found matches occurring than the previous, there's no
  ;; reason to move the search position along, since it would be changing
  ;; anyway, because of cutting the subtree-at-point.
  (today-move--unfinished-checkboxes-to-date destination-date)
  (with-current-buffer (today-fs-buffer-from-date source-date)
    (while (string-match today-move--unfinished-task-regexp (buffer-string))
      (if (>= (match-beginning 0) 0)
          (progn
            ;; move 1 character into the found line, to make sure we're on the
            ;; correct line, and not on the last character of the previous line.
            (goto-char (+ 1 (match-beginning 0)))
            (today-move--subtree-action destination-date))))))

;;;###autoload
(defun today-move-unfinished-to-tomorrow ()
  "Move all unfinished tasks in the current buffer, to tomorrows
file."
  (interactive)
  (letrec ((current-files-date (today-util-current-files-date))
           (tomorrows-date (today-util-date-add-days current-files-date 1)))
    (today-move--unfinished-to-date-action current-files-date tomorrows-date)))

;;;###autoload
(defun today-move-unfinished-from-previous ()
  "Move all unfinished tasks from the previous file, to the
current file."
  (interactive)
  (letrec ((current-files-date (today-util-current-files-date))
           (earlier-dates (today-util-dates-earlier-than current-files-date))
           (previous-files-date (car earlier-dates)))
    (if previous-files-date
        (today-move--unfinished-to-date-action previous-files-date current-files-date)
      (message "No previous file found!"))))

;;;###autoload
(defun today-move-unfinished-to-date (arg)
  "Move unfinished tasks from the current file to a new date. If
called with a prefix argument, move to the file n-days relative
to the current file, otherwise prompt for a date using
`org-calendar'."
  (interactive "P")
  (letrec ((date (if arg
                     (today-util-date-add-days (today-util-current-files-date) arg)
                   (org-read-date)))
           (next-date (today-util-date-add-days date 1)))
    (today-move--unfinished-to-date-action date next-date)))

(defun today-move--unfinished-checkboxes-to-date (date)
  "Move all unfinished checkboxes to DATEs file. removing them
from the current file. The destination file will be created if it
does not exist, as will the containing task."
  (interactive)
  (save-excursion
    (letrec ((pos 0)
             (empty-checkbox-regex "^\\- \\[ \\]")
             (completed-checkbox-regex "^\\- \\[X\\]"))
      (while (string-match empty-checkbox-regex (buffer-string) pos)
        (when (>= (match-beginning 0) 0)
          ;; go to the heading containing the found checkboxes
          (goto-char (match-beginning 0))
          (outline-previous-heading)

          ;; copy the entire subtree to the new file, then remove checked
          ;; checkboxes
          (org-copy-subtree)
          (with-current-buffer (today-fs-buffer-from-date date)
            ;; insert at the end of the file
            (goto-char (point-max))
            (yank)
            ;; then jump back up, remove the checked checkboxes, and update the
            ;; checkbox status
            (outline-previous-heading)
            (delete-matching-lines completed-checkbox-regex)
            (org-update-checkbox-count)
            (save-buffer))

          ;; remove the unchecked checkboxes from the source entry, and update
          ;; its TODO-state, and the checkbox count
          (org-mark-subtree)
          (delete-matching-lines empty-checkbox-regex (region-beginning) (region-end) 't)
          (org-todo)
          (org-update-checkbox-count)

          ;; go to the next entry and continue searching
          (outline-next-heading)
          (setq pos (- (point) 1))))
      (save-buffer))))

;;;###autoload
(defun today-move-unfinished-checkboxes-to-tomorrow ()
  "Move unfinished checkboxes to tomorrows file, creating the
containing task/file if it does not exist."
  (interactive)
  (today-move--unfinished-checkboxes-to-date
   (today-util-date-add-days (today-util-current-files-date) 1)))

;;;###autoload
(defun today-move-unfinished-checkboxes-to-date ()
  "Move unfinished checkboxes to DATEs file, creating the
containing task/file if it does not exist."
  (interactive)
  (let ((date (org-read-date)))
    (today-move--unfinished-checkboxes-to-date
     (today-util-date-add-days date 1))))

(provide 'today-move)

;;; -*- lexical-binding: t -*-

(require 'today-fs)
(require 'today-util)

(defcustom today-move--unfinished-task-regexp "^\\* TODO "
  "The regexp used to search for a incomplete tasks.")

(defcustom today-move--completed-task-regexp "^\\* DONE "
  "The regexp used to search for a completed tasks.")

(defun today-move--subtree-action (date)
  "Move the subtree-at-point, to the bottom of the file
corresponding to DATE."
  (let ((subtree (today-util-cut-subtree-at-point)))
    (today-util-insert-entry subtree date)))

(defun today-move--unfinished-checkboxes-to-date (date)
  "Move all unfinished checkboxes to DATEs file. removing them
from the current file. The destination file will be created if it
does not exist, as will the containing task."
  (interactive)
  (save-excursion
    (letrec ((pos 0)
             (checkbox-regex "\\[[0-9]+\\/[0-9]+\\]$")
             (empty-checkbox-entry "\\[0\\/0\\]$"))
      (while (string-match checkbox-regex (buffer-string) pos)
        (when (>= (match-beginning 0) 0)
          ;; go to the heading containing the found checkboxes
          (goto-char (match-beginning 0))

          (letrec ((subtree (today-util-copy-subtree-at-point))
                   (subtree-without-empties (today-util-remove-checkboxes-from-subtree subtree 'empty))
                   (subtree-without-checked (today-util-remove-checkboxes-from-subtree subtree 'checked)))
            (today-util-insert-entry subtree-without-checked date)
            (org-cut-subtree)
            (insert subtree-without-empties))

          ;; go to the next entry and continue searching
          (setq pos (- (point) 1))))
      ;; remove heading of checkbox entries where all items have been moved.
      (goto-char (point-min))
      (delete-matching-lines empty-checkbox-entry)
      (save-buffer))))

;;;###autoload
(defun today-move-completed-to-date (date)
  "Move completed tasks to DATEs file."
  (save-excursion
    (while (string-match today-move--completed-task-regexp (buffer-string))
      (when (>= (match-beginning 0) 0)
        ;; move 1 character into the found line, to make sure we're on the
        ;; correct line, and not on the last character of the previous line.
        (goto-char (+ 1 (match-beginning 0)))
        (today-move--subtree-action date)))))

;;;###autoload
(defun today-move-archive-completed ()
  "Move all complated tasks in the current buffer to todays
file."
  (interactive)
  (letrec ((todays-date (today-util-todays-date)))
    (today-move-completed-to-date todays-date)))

(provide 'today-move)

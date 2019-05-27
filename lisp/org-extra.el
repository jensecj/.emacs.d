;;; org-extra.el --- Extra utilities for org-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: org-mode
;; Package-Version: 20190526
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun org-extra-subtree-remove-checkboxes (subtree &optional checkbox-state)
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

(defun org-extra-copy-subtree-at-point ()
  "Return a copy of the current subtree-at-point."
  (org-copy-subtree)
  (with-temp-buffer
    (yank)
    (buffer-string)))

(defun org-extra-cut-subtree-at-point ()
  "Return the current subtree-at-point, cutting from the
document."
  (org-cut-subtree)
  (with-temp-buffer
    (yank)
    (buffer-string)))

(defun org-extra-copy-url-at-point ()
  "Grab URL from org-link at point."
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 (buffer-substring-no-properties (cadr link-info) (caddr link-info)))))
    (if (not text)
        (error "Not in org link")
      (with-temp-buffer
        (string-match "\\[\\[.*\\]\\[" text)
        (insert (substring-no-properties text (+ (match-beginning 0) 2) (- (match-end 0) 2)))
        (clipboard-kill-ring-save (point-min) (point-max))
        (buffer-string)))))

(provide 'org-extra)

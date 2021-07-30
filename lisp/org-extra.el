;;; org-extra.el --- Extra utilities for org-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@subst.net>
;; Keywords: org-mode
;; Package-Version: 20200615
;; Version: 0.3.0

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
(require 'dash)
(require 's)

(require 'org)
(require 'rx)

;; * helpers

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

(defun org-extra-url-at-point ()
  "Return the URL from the org-link-at-point"
  (when-let* ((link-info (assoc :link (org-context)))
              (text (buffer-substring-no-properties (cadr link-info) (caddr link-info))))
    (if (not text)
        (error "Not in org link")
      (with-temp-buffer
        (string-match "\\[\\[.*\\]\\[" text)
        (insert (substring-no-properties text (+ (match-beginning 0) 2) (- (match-end 0) 2)))
        (buffer-string)))))

;; * commands

(defun org-extra-move-subtree-at-point-to-top ()
  "Move the subtree at point to the top of the current buffer."
  (interactive)
  (save-excursion
    (when (org-at-heading-p)
      (org-cut-subtree)
      (goto-char (point-min))
      (newline)
      (forward-line -1)
      (org-paste-subtree))))

(defun org-extra-move-subtree-at-point-to-bottom ()
  "Move the subtree at point to the bottom of the current buffer."
  (interactive)
  (save-excursion
    (when (org-at-heading-p)
      (org-cut-subtree)
      (goto-char (point-max))
      (newline)
      (org-paste-subtree))))

(defun org-extra-copy-url-at-point ()
  "Grab URL from org-link at point."
  (interactive)
  (when-let* ((url (org-extra-url-at-point)))
    (with-temp-buffer
      (insert url)
      (clipboard-kill-ring-save (point-min) (point-max))
      (message "%s" (buffer-string)))))

(defun org-extra-open-link-at-point ()
  ;; TODO: remove #'org-extra-open-link-at-point
  "Open the org-link at point."
  (interactive)
  (when-let* ((line-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (org-link-re (rx "[[" (group (zero-or-more any))  "][" (group (zero-or-more any)) "]]"))
              (found (string-match org-link-re line-string))
              (url (substring line-string (match-beginning 0) (match-end 0))))
    (org-open-link-from-string url)))

(defun org-extra-url-at-point-to-org-link ()
  "Convert the url-at-point to an org-link."
  (interactive)
  (when-let* ((url (thing-at-point 'url))
              (bounds (bounds-of-thing-at-point 'url))
              (entry (today-capture--url-to-org-link url)))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (insert entry))))

(defun org-extra-agenda-here ()
  (interactive)
  (let* ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list (* 7 3))))

(defun org-extra-rate ()
  "Rate an org-heading on a scale from 1-10"
  (interactive)
  (let ((ivy-sort-functions-alist nil))
    (when-let* ((values (-map #'number-to-string (number-sequence 1 10)))
                (rating (completing-read "rate: " values nil t)))
      (org-set-property "RATING" rating))))


(provide 'org-extra)

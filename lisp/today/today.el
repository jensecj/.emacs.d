;;; today.el --- simple daily planning. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: org, org-mode, planning, today, todo
;; Package-Version: 20190404
;; Version: 0.9
;; Package-Requires: ((emacs "25.1") (org "9.0") (dash "2.14.1")  (s "1.12.0") (f "0.20.0") (hydra "0.14.0") (org-web-tools "0.1.0-pre"))

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

;; A simple tool to capture links to your org inbox.

;; A collection of commands that enable a certain workflow of creating,
;; manipulating, and ordering files, to enable low-hassle daily planning.

;;; Notes on structure:

;; Entries are collected in a central file, `today-file', and when completed
;; each entry can be archived as a file in a directory inside of
;; `today-directory', whose name is the date for that entry. The org file for
;; the entry resides in this directory, and is also named with the date, ending
;; with the `.org' extension.  This is done so that each entry can have its own
;; extra content (e.g. images, pdfs), reside in its directory and not interfere
;; with the other entries.

;;; Code:

(require 'hydra)

(defcustom today-directory
  (concat user-emacs-directory "today-planner")
  "Directory used for planning files.")

(defcustom today-file
  "today.org"
  "Accumulating file for today entries.")

(require 'today-capture)

(defun today--path-from-date (date)
  "Returns the path to the file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today--visit-date-file (date)
  "Visit the file for DATE, create it if it does not exist."
  (let ((date-file (today--path-from-date date)))
    (find-file date-file)))

;;;###autoload
(defun today ()
  "Visit the today file, containing entries."
  (interactive)
  (find-file (concat today-directory today-file)))

;;;###autoload
(defun today-visit-todays-file ()
  "Visit today's file, create it if it does not exist."
  (interactive)
  (xref-push-marker-stack)
  (today--visit-date-file (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun today-list ()
  "List all files from `today-directory', visit the one
selected."
  (interactive)
  (let* ((ivy-sort-functions-alist nil) ;; dates are already sorted
         (dates (reverse (-map #'f-base (f-directories today-directory))))
         (date (completing-read "Date: " dates)))
    (xref-push-marker-stack)
    (today--visit-date-file date)))

(defun today-set-rating ()
  "Set a 0-10 rating for the current entry."
  (interactive)
  (let ((ivy-sort-functions-alist nil)
        (rating-re (rx (1+ (or "★" "☆")))))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward rating-re (line-end-position) t 1)
          (let* ((rating-start (match-beginning 0))
                 (rating-end (match-end 0))
                 (rating (completing-read "rating: " (-map #'number-to-string (number-sequence 0 10)) nil t))
                 (rating (string-to-number rating)))
            (delete-region rating-start rating-end)
            (goto-char rating-start)
            (insert (s-repeat rating "★") (s-repeat (- 10 rating) "☆")))
        (message "no ratings found on this line.")))))

(defun today-get-dates-field ()
  "Return the bounds and dates for the dates-field on the current line."
  (let* ((date-pattern (rx
                        "["
                        (group-n 1
                                 (zero-or-one
                                  (repeat 4 digit) "-"
                                  (repeat 2 digit) "-"
                                  (repeat 2 digit)))
                        "]"
                        "--"
                        "["
                        (group-n 2
                                 (zero-or-one
                                  (repeat 4 digit) "-"
                                  (repeat 2 digit) "-"
                                  (repeat 2 digit)))
                        "]"
                        )))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward date-pattern (line-end-position) t 1)
          (let* ((dates (car (s-match-strings-all date-pattern (buffer-substring (line-beginning-position) (line-end-position)))))
                 (start-date (nth 1 dates))
                 (end-date (nth 2 dates)))
            (list (cons (match-beginning 0) (match-end 0)) start-date end-date))))))

(defun today-set-dates-field (bounds start-date end-date)
  "Set START-DATE and END-DATE for the dates-field in BOUNDS."
  (interactive)
  (save-excursion
    (delete-region (car bounds) (cdr bounds))
    (goto-char (car bounds))
    (insert (format "[%s]--[%s]" start-date end-date))))

(defun today-set-started ()
  "Set the started part of the dates field to today."
  (interactive)
  (let ((bounds-and-dates (today-get-dates-field))
        (start-date (format-time-string "%Y-%m-%d")))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (end-date (nth 2 bounds-and-dates)))
          (today-set-dates-field bounds start-date end-date)))))

(defun today-clear-started ()
  "Clear the started part of the dates field."
  (interactive)
  (let ((bounds-and-dates (today-get-dates-field)))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (end-date (nth 2 bounds-and-dates)))
          (today-set-dates-field bounds "" end-date)))))

(defun today-set-completed ()
  "Set the completed part of the dates field to today."
  (interactive)
  (let ((bounds-and-dates (today-get-dates-field))
        (end-date (format-time-string "%Y-%m-%d")))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (start-date (nth 1 bounds-and-dates)))
          (today-set-dates-field bounds start-date end-date)))))

(defun today-clear-completed ()
  "Clear the completed part of the dates field."
  (interactive)
  (let ((bounds-and-dates (today-get-dates-field)))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (start-date (nth 1 bounds-and-dates)))
          (today-set-dates-field bounds start-date "")))))

(defhydra today-set-hydra (:foreign-keys run)
  ("s" (today-set-started) "start")
  ("S" (today-clear-started) "clear start")

  ("c" (today-set-completed) "complete")
  ("C" (today-clear-completed) "clear complete")

  ("r" (today-set-rating) "rate")

  ("q" nil "quit"))

(provide 'today)

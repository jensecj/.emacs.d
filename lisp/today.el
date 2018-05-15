;;; -*- lexical-binding: t -*-
;;; today.el --- simple daily planning

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: fullscreen
;; Package-Version: 20180515
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

;; a simple daily planner, using org-mode.
;; `today' will create a new planning file for the current date.
;; `today-move-to-tomorrow' will move the subtree-at-point to tomorrows file.
;; `today-move-to-date' will prompt for a date using the org-calendar, and
;; move the subtree-at-point to the planning file for that date.
;; `today-go-to' will prompt for a date using the org-calendar, then jump to the
;; corresponding planning file for that date.
;; `today-list' will list the dates for all planning files in `today-directory',
;; selecting one will jump to the corresponding file.

;; you can add all the planning files to `org-agenda-files' by adding the
;; `today-directory', and then changing `org-agenda-file-regex' to match the
;; subdirectories of `today-directory'.
;; a less intrusive way is to hook `org-agenda', and find all the .org files
;; recursively on each invocation. (note: this will be slow for big collections
;; for files)

;;; Code:

(require 'f)
(require 'dash)

(defcustom today-directory
  (concat user-emacs-directory "planning")
  "Directory used for planning files.")

(defun today--file-from-date (date)
  "Planning file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today--create-path (path)
  "Creates a planning file with PATH."
  ;; create the planning directory if it does not exist
  (unless (f-exists? today-directory)
    (f-mkdir today-directory))

  (let ((dir (f-dirname path)))
    ;; create directory of PATH if it does not exist
    (unless (f-exists? dir)
      (f-mkdir dir)))

  ;; finally, create the file of PATH
  (unless (f-exists? path)
    (f-touch path)))

(defun today--find-file-by-date (date)
  "Jump to the planning file for DATE, create it if it does not exist."
  (letrec ((file (today--file-from-date date)))
    (today--create-path file)
    (find-file file)))

(defun today ()
  "Open todays planning file, create it if it does not exist."
  (interactive)
  (letrec ((todays-date (format-time-string "%Y-%m-%d")))
    (today--find-file-by-date todays-date)))

(defun today-list ()
  "List all dates from `today-directory', jump to the one selected."
  (interactive)
  (letrec ((dates (-map #'f-base (f-directories today-directory)))
           (date (completing-read "Date: " dates)))
    (today--find-file-by-date date)))

(defun today-go-to ()
  "Prompt for date, and go to corresponding planning file."
  (interactive)
  (letrec ((date (org-read-date)))
    (today--find-file-by-date date)))

(defun today-move-to-tomorrow ()
  "Move a subtree from a planning file to tomorrows planning file."
  (interactive)
  (letrec ((todays-date-in-seconds (float-time))
           (seconds-in-a-day (* 60 60 24))
           (tomorrows-date-in-seconds (+ todays-date-in-seconds seconds-in-a-day))
           (tomorrows-date (format-time-string "%Y-%m-%d" tomorrows-date-in-seconds))
           (tomorrows-file (today--file-from-date tomorrows-date)))
    (today--create-path tomorrows-file)

    (org-cut-subtree)

    (save-excursion
      (let ((tomorrows-buffer (find-file-other-window tomorrows-file)))
        (with-current-buffer tomorrows-buffer
          (org-mode)
          (goto-char (point-max))
          (yank))))))

(defun today-move-to-date ()
  "Moves a subtree to the selected date."
  (interactive)
  (letrec ((date (org-read-date))
           (new-file (today--file-from-date date)))
    (today--create-path new-file)

    (org-cut-subtree)

    (with-current-buffer (find-file new-file)
      (org-mode)
      (goto-char (point-max))
      (yank))))

(provide 'today)

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
;; `today' will visit the planning file for the current date, creates it if it does not exist.
;; `today-move-to-tomorrow' will move the subtree-at-point to tomorrows planning file.
;; `today-move-to-date' will prompt for a date using the org-calendar, and
;; move the subtree-at-point to the planning file for that date.
;; `today-go-to' will prompt for a date using the org-calendar, then jump to the
;; planning file for that date.
;; `today-list' will list the dates for all planning files in `today-directory',
;; selecting one will jump to that file.

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
  (concat user-emacs-directory "today-planner")
  "Directory used for planning files.")

(defcustom today-capture-tasks
  '("read" "watch")
  "Tasks that can be captured by `today-capture' functions")

(defun today--todays-date ()
  "Today's date."
  (format-time-string "%Y-%m-%d"))

(defun today--date-add-days (date days)
  (letrec ((date-in-seconds (float-time (date-to-time (concat date " 12:00:00 EST"))))
           (seconds-in-a-day (* 60 60 24))
           (total-seconds-in-days (* days seconds-in-a-day))
           (next-date-in-seconds (+ date-in-seconds total-seconds-in-days))
           (next-date (format-time-string "%Y-%m-%d" next-date-in-seconds)))
    next-date))

(defun today--file-from-date (date)
  "Planning file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today--create-planning-file (filepath)
  "Creates a planning file with PATH."
  ;; create the planning directory if it does not exist
  (unless (f-exists? today-directory)
    (f-mkdir today-directory))

  (let ((dir (f-dirname filepath)))
    ;; create directory of PATH if it does not exist
    (unless (f-exists? dir)
      (f-mkdir dir)))

  ;; finally, create the file of PATH
  (unless (f-exists? filepath)
    (f-touch filepath)))

(defun today--buffer-from-date (date)
  "Get the buffer for DATEs planning file, creates one if it does not exist."
  (let ((file (today--file-from-date date)))
    (today--create-planning-file file)
    (find-file-noselect file)))

(defun today--visit-date-file (date)
  "Jump to the planning file for DATE, create it if it does not exist."
  (switch-to-buffer (today--buffer-from-date date)))

(defun today ()
  "Open todays planning file, create it if it does not exist."
  (interactive)
  (today--visit-date-file (today--todays-date)))

(defun today--capture (date task entry)
  "Captures an ENTRY with TASK, into the planning file for DATE."
  (save-excursion
    (with-current-buffer (today--buffer-from-date date)
      (end-of-buffer)
      (newline)
      (insert "* TODO " task " " entry))))

(defun today-capture (task entry)
  "Capture ENTRY with TASK into todays planning file."
  (today--capture (today--todays-date) task entry))

(defun today-capture-with-task (task)
  "Prompt for ENTRY, then capture with TASK into todays planning file."
  (letrec ((link (completing-read "link: " '()))
           (org-link (link-to-org-link link)))
    (today-capture task org-link)))

(defun today-capture-prompt ()
  "Captures a LINK into todays planning file, with the selected TASK."
  (interactive)
  (letrec ((task (completing-read "task: " today-capture-tasks))
           (link (completing-read "link: " '()))
           (org-link (link-to-org-link link)))
    (today-capture task org-link)))

(defun today-list ()
  "List all dates from `today-directory', jump to the one selected."
  (interactive)
  (letrec ((dates (-map #'f-base (f-directories today-directory)))
           (date (completing-read "Date: " dates)))
    (today--visit-date-file date)))

(defun today-goto-date ()
  "Prompt for date, and go to corresponding planning file."
  (interactive)
  (letrec ((date (org-read-date)))
    (today--visit-date-file date)))

(defun today--move-subtree-action (destination-file)
  "Move the org subtree at point, to the bottom of DESTINATION-FILE."
  (org-cut-subtree)
  (save-buffer)

  (with-current-buffer (find-file destination-file)
    (org-mode)
    (goto-char (point-max))
    (yank)
    (save-buffer)
    (previous-buffer)))

(defun today-move-to-tomorrow ()
  "Move a subtree from a planning file to tomorrows planning file."
  (interactive)
  (letrec ((todays-date-in-seconds (float-time))
           (current-files-date (f-base (f-no-ext (buffer-file-name))))
           (tomorrows-date (today--date-add-days current-files-date 1))
           (tomorrows-file (today--file-from-date tomorrows-date)))
    (today--create-planning-file tomorrows-file)
    (today--move-subtree-action tomorrows-file)))

(defun today-move-to-date ()
  "Moves a subtree to the selected date."
  (interactive)
  (letrec ((date (org-read-date))
           (new-file (today--file-from-date date)))
    (today--create-planning-file new-file)
    (today--move-subtree-action new-file)))

(defun today--find-unfinished ()
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((unfinished-subtree-regexp "^* TODO")
          (content (buffer-string))
          (pos 0)
          matches)
      (while (string-match unfinished-subtree-regexp content pos)
        (push pos matches)
        (setq pos (match-end 0)))
      matches)))

(defun today--move-unfinished-to-date-action (date)
  (letrec ((date-file (today--file-from-date date)))

    (while (string-match "^\\* TODO" (buffer-string))
      (message (buffer-string))
      (if (>= (match-beginning 0) 0)
          (progn
            (goto-char (+ 1 (match-beginning 0)))
            (today--move-subtree-action date-file))))))

(defun today-move-unfinished-to-tomorrow ()
  (interactive)
  (letrec ((current-files-date (f-base (f-no-ext (buffer-file-name))))
           (current-files-time-in-seconds (float-time (date-to-time (concat current-files-date " 12:00:00 EST"))))
           (tomorrows-date (today--date-add-days current-files-date 1))
           (tomorrows-file (today--file-from-date tomorrows-date)))
    (today--create-planning-file tomorrows-file)
    (today--move-unfinished-to-date-action tomorrows-date)))

(defun today-move-unfinished-to-date ()
  (interactive)
  (letrec ((date (org-read-date))
           (next-date (today--date-add-days date 1))
           (next-file (today--file-from-date next-date)))
    (today--create-planning-file next-file)
    (today--move-unfinished-to-date-action next-date)))

(provide 'today)

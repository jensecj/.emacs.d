;;; today.el --- simple daily planning

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: fullscreen
;; Package-Version: 20180513
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

;; a simple daily planner, using `today' will create a new planning file for
;; the current date.
;; a subtree can be moved to tomorrows file with `today-move-to-tomorrow'.

;;; Code:

(require 'f)

(defcustom today-directory
  (concat user-emacs-directory "planning")
  "Directory used for planning files.")

(defun today--current-date ()
  "Date string for the current date."
  (format-time-string "%Y-%m-%d"))

(defun today--file-from-date (date)
  "Planning file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today--create-file (path)
  "Creates a planning file with PATH."
  (unless (f-exists? path)
    (f-touch path)))

(defun today ()
  "Open todays planning file, create it if it does not exist."
  (interactive)
  ;; create the planning directory if it does not exist
  (unless (f-directory? today-directory)
    (f-mkdir today-directory))

  ;; create todays directory if it does not exist
  (unless (f-directory? today-directory)
    (f-mkdir today-directory))

  (letrec ((todays-date (today--current-date))
           (todays-file (today--file-from-date todays-date)))
    (today--create-file todays-file)
    (find-file todays-file)))

(defun today-move-to-tomorrow ()
  "Move a subtree from a planning file to tomorrows planning file."
  (interactive)
  (letrec ((todays-date-in-seconds (float-time))
           (seconds-in-a-day (* 60 60 24))
           (tomorrows-date-in-seconds (+ todays-date-in-seconds seconds-in-a-day))
           (tomorrows-date (format-time-string "%Y-%m-%d" tomorrows-date-in-seconds))
           (tomorrows-file (today--file-from-date tomorrows-date)))
    (today--create-file tomorrows-file)

    (org-cut-subtree)

    (let ((tomorrows-buffer (find-file tomorrows-file)))
      (with-current-buffer tomorrows-buffer
        (org-mode)
        (goto-char (point-max))
        (yank)))))

(provide 'today)

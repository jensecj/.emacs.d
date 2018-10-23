;;; -*- lexical-binding: t -*-
;;; today.el --- simple daily planning

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: org, org-mode, planning, today, todo
;; Package-Version: 20180920
;; Version: 0.6
;; Package-Requires: ((emacs "25.1") (org "9.0") (dash "2.14.1") (f "0.20.0") (hydra "0.14.0") (org-web-tools "0.1.0-pre"))

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

;; A simple daily planner, using org-mode.

;; A collection of commands that enable a certain workflow of creating,
;; manipulating, and ordering files, to enable low-hassle daily planning.

;; All the planning files can be added to `org-agenda-files' by appending
;; `today-directory', and then changing `org-agenda-file-regex' to match the
;; sub-directories of `today-directory'.  A less intrusive way is to hook
;; `org-agenda', and find all the .org files recursively on each invocation.

;; (require 'f)
;; (defun load-org-agenda-files ()
;;   (interactive)
;;   (setq org-agenda-files
;;         (append '("")
;;                 (f-glob "**/*.org" "~/org/today-planner"))))

;; (advice-add 'org-agenda :before #'load-org-agenda-files)

;; (note: this will be slow for big collections for files):

;;; Notes on structure:

;; Entries are collected in a central file, `today-file', and when completed
;; each entry is archived as a file in a directory inside of `today-directory',
;; whose name is the date for that entry. The org file for the entry resides in
;; this directory, and is also named with the date, ending with the `.org'
;; extension.  This is done so that each entry can have its own extra content
;; (e.g. images, pdfs), reside in its directory and not interfere with the other
;; entries.

;;; Code:

(defcustom today-directory
  (concat user-emacs-directory "today-planner")
  "Directory used for planning files.")

(defcustom today-file
  "today.org"
  "Accumulating file for today entries.")

(require 'today-fs)
(require 'today-util)
(require 'today-capture)
(require 'today-move)

;;;###autoload
(defun today ()
  "Visit the today file, containing entries."
  (interactive)
  (find-file (concat today-directory today-file)))

;;;###autoload
(defun today-visit-todays-file ()
  "Visit today's file, create it if it does not exist."
  (interactive)
  (today-fs-visit-date-file (today-util-todays-date)))

;;;###autoload
(defun today-list ()
  "List all files from `today-directory', visit the one
selected."
  (interactive)
  (letrec ((dates (today-util-list-files))
           (date (completing-read "Date: " dates)))
    (today-fs-visit-date-file date)))

;;;###autoload
(defun today-goto-date ()
  "Prompt for date using `org-calendar', then visit the
corresponding file."
  (interactive)
  (letrec ((date (org-read-date)))
    (today-fs-visit-date-file date)))

(defun today-visit-previous ()
  "Visit the previous file, based on the current files date."
  (interactive)
  (letrec ((current-date (today-util-current-files-date))
           (previous-date (car (today-util-dates-earlier-than current-date))))
    (if (null previous-date)
        (today-fs-visit-date-file (today-util-date-add-days current-date -1))
      (today-fs-visit-date-file previous-date))))

(defun today-visit-next ()
  "Visit the next file, based on the current files date."
  (interactive)
  (letrec ((current-date (today-util-current-files-date))
           (next-date (car (today-util-dates-later-than current-date))))
    (if (null next-date)
        (today-fs-visit-date-file (today-util-date-add-days current-date +1))
      (today-fs-visit-date-file next-date))))

;; This hydra will exit on one-off commands, such as `today-list', or
;; `today-goto-date', but will persist when using capture or movement commands.
(require 'hydra)
(defhydra today-hydra (:foreign-keys run)
  "
^Capture^                                 ^Actions^                          ^Find^
^^^^^^^^----------------------------------------------------------------------------------------------
_r_: capture read task                    _a_: archive completed tasks      _t_: go to todays file
_R_: capture read task from clipboard     ^ ^                               _l_: list all date files
_w_: capture watch task                   ^ ^                               _g_: go to date from `org-calendar'
_W_: capture watch task from clipboard    ^ ^                               ^ ^
_c_: capture with prompt                  ^ ^                               ^ ^
"
  ("r" (lambda () (interactive) (today-capture-link-with-task 'read)))
  ("R" (lambda () (interactive) (today-capture-link-with-task-from-clipboard 'read)))
  ("w" (lambda () (interactive) (today-capture-link-with-task 'watch)))
  ("W" (lambda () (interactive) (today-capture-link-with-task-from-clipboard 'watch)))
  ("c" #'today-capture-prompt)

  ("a" #'today-move-archive-completed :exit t)

  ("t" #'today :exit t)
  ("T" #'today-visit-todays-file :exit t)

  ("l" #'today-list :exit t)
  ("g" #'today-goto-date :exit t)

  ("q" nil "quit"))

(provide 'today)

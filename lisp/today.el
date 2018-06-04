;;; -*- lexical-binding: t -*-
;;; today.el --- simple daily planning

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: org, org-mode, planning, today, todo
;; Package-Version: 20180604
;; Version: 0.3
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

;; Each entry is a directory inside of `today-directory', whose name is the date
;; for that entry. The org file for the entry resides in this directory, and is
;; also named with the date, ending with the `.org' extension.  This is done so
;; that each entry can have its own extra content, e.g. images, or other files,
;; reside in its directory and not interfere with the other entries.  `today'
;; tries to create the planning file which is the target of a command, e.g. when
;; using `today-move-to-tomorrow' (or other similar commands), the file for
;; tomorrows date will be created if it does not exist.  `today' also tries to
;; figure out the proper date for a command, e.g. if you're visiting a planning
;; file which is not for today's date, using `today-move-to-tomorrow' (or any
;; other movement commands) will figure out the correct date in relation to the
;; visited file.

;;; Code:

(require 'f)
(require 'dash)
(require 'org-web-tools)

;;;;;;;;;;;;;;;
;; Web Utils ;;
;;;;;;;;;;;;;;;

(defun today--get-website-title-from-link (link)
  "Try to retrieve the website title from a link, requires `org-web-tools'."
  (with-current-buffer (url-retrieve-synchronously link)
    (org-web-tools--html-title (buffer-string))))

(defun today--get-website-lines-from-link (link)
  "Try to retrieve the number of lines on the website of LINK. requires `lynx'."
  (let ((lines (s-trim (shell-command-to-string (format "lynx -dump %s | wc -l" link)))))
    (if (< (length lines) 5) lines "?")))

(defun today--to-org-link (link title)
  "Convert a link and its title into `org-link' format."
  (format "[[%s][%s]]" link title))

(defun today--link-to-org-link (link)
  "Try to get the website title of LINK, then convert into
`org-link' format."
  (let ((title (today--get-website-title-from-link link)))
    (to-org-link link title)))

;;;;;;;;;;;;;;;;
;; Date Utils ;;
;;;;;;;;;;;;;;;;

(defun today--todays-date ()
  "Today's date."
  (format-time-string "%Y-%m-%d"))

(defun today--current-files-date ()
  "Get the date of the current file. See `Notes on structure' for
explanation."
  (f-base (f-no-ext (buffer-file-name))))

(defun today--date-add-days (date days)
  "Return the date DAYS after DATE."
  (letrec ((date-in-seconds (float-time (date-to-time (concat date " 12:00:00 EST"))))
           (seconds-in-a-day (* 60 60 24))
           (total-seconds-in-days (* days seconds-in-a-day))
           (next-date-in-seconds (+ date-in-seconds total-seconds-in-days))
           (next-date (format-time-string "%Y-%m-%d" next-date-in-seconds)))
    next-date))

;;;;;;;;;;
;; Main ;;
;;;;;;;;;;

(defcustom today-directory
  (concat user-emacs-directory "today-planner")
  "Directory used for planning files.")

(defcustom today-capture-tasks
  '(read watch)
  "Tasks that can be captured by `today-capture' functions")

(defun today--capture-read-link-handler (content)
  "Handler for READ task. Expects CONTENT to be a link to some
website. Will try to extract the number of lines on the website,
and add to the front of the entry. Will also try to extract the
title of the website, and convert the link into an `org-mode'
link, using the title."
  (let ((lines (today--get-website-lines-from-link content))
        (org-link (today--link-to-org-link content)))
    (format "read (%s lines) %s" lines org-link)))

(defun today--capture-watch-link-handler (link)
  "Handler for the WATCH task. Expects CONTENT to be a link to a
website, will try to extract the title of the website, and create
an `org-mode' link using that title."
  (letrec ((title (today--get-website-title-from-link link))
           (entry (today--to-org-link link title)))
    (format "watch %s" entry)))

(defvar today-capture-handlers-alist
  '((read . today--capture-read-link-handler)
    (watch . today--capture-watch-link-handler))
  "List of capture tasks and their associated handlers. A handler
  recieves the capture content as a parameter.")

(defun today--file-from-date (date)
  "Returns the path to the file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today--create-planning-file (filepath)
  "Creates a planning file with FILEPATH."
  ;; create the planning directory if it does not exist
  (unless (f-exists? today-directory)
    (f-mkdir today-directory))

  (let ((dir (f-dirname filepath)))
    ;; create directory of FILEPATH if it does not exist
    (unless (f-exists? dir)
      (f-mkdir dir)))

  ;; finally, create the file of FILEPATH
  (unless (f-exists? filepath)
    (f-touch filepath)))

(defun today--buffer-from-date (date)
  "Get the buffer for DATEs file, if the buffer does not exist,
then visit the corresponding file, if the file does not exist,
then create it."
  (let ((file (today--file-from-date date)))
    (today--create-planning-file file)
    (find-file-noselect file)))

(defun today--visit-date-file (date)
  "Visit the file for DATE, create it if it does not exist."
  (switch-to-buffer (today--buffer-from-date date)))

;;;###autoload
(defun today ()
  "Visit today's file, create it if it does not exist."
  (interactive)
  (today--visit-date-file (today--todays-date)))

(defun today--apply-handler (task entry)
  "If a handler exists for TASK, then return the result of
applying handler on ENTRY, otherwise return ENTRY."
  (let ((handler (assoc task today-capture-handlers-alist)))
    (if handler
        (funcall (cdr handler) entry)
      entry)))

;;;###autoload
(defun today-capture-to-date (date task entry)
  "Captures an ENTRY with TASK, into the file for DATE."
  (let ((content (today--apply-capture-handler task entry)))
    (with-current-buffer (today--buffer-from-date date)
      (end-of-buffer)
      (newline)
      (insert "* TODO " content))))

;;;###autoload
(defun today-capture (task entry)
  "Capture ENTRY with TASK into todays file."
  (today-capture-to-date (today--todays-date) task entry))

;;;###autoload
(defun today-capture-link-with-task (task)
  "Prompt for ENTRY, then capture with TASK into today's file."
  (letrec ((link (completing-read "link: " '())))
    (today-capture task link)))

;;;###autoload
(defun today-capture-prompt ()
  "Captures a LINK into today's file, with the selected TASK."
  (interactive)
  (letrec ((task (completing-read "task: " today-capture-tasks))
           (entry (completing-read "entry: " '())))
    (today-capture task entry)))

(defun today-capture-elfeed-at-point ()
  "Captures a TASK from selected elfeed entry."
  (interactive)
  (letrec ((entry (car (elfeed-search-selected)))
           (link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (org-link (today--to-org-link link title)))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (today-capture 'elfeed org-link)
    (next-line)))

(defun today--list-of-files ()
  "Get the list of all planning files, from newest to oldest."
  (reverse (-map #'f-base (f-directories today-directory))))

;;;###autoload
(defun today-list ()
  "List all files from `today-directory', visit the one
selected."
  (interactive)
  (letrec ((dates (today--list-of-files))
           (date (completing-read "Date: " dates)))
    (today--visit-date-file date)))

;;;###autoload
(defun today-goto-date ()
  "Prompt for date using `org-calendar', then visit the
corresponding file."
  (interactive)
  (letrec ((date (org-read-date)))
    (today--visit-date-file date)))

(defun today--move-subtree-action (date)
  "Move the subtree-at-point, to the bottom of the file
corresponding to DATE."
  (org-cut-subtree)
  (save-buffer)
  (with-current-buffer (today--buffer-from-date date)
    (org-mode)
    (goto-char (point-max))
    (yank)
    (save-buffer)))

;;;###autoload
(defun today-move-to-tomorrow ()
  "Move the subtree-at-point to tomorrows file."
  (interactive)
  (letrec ((current-files-date (today--current-files-date))
           (tomorrows-date (today--date-add-days current-files-date 1)))
    (today--move-subtree-action tomorrows-date)))

;;;###autoload
(defun today-move-to-date (arg)
  "Move the subtree-at-point to a date selected with
`org-calendar', or, if using a prefix argument, move it n-days
relative to the current file. (also works with negative
prefixes)"
  (interactive "P")
  (letrec ((date (if arg
                     (today--date-add-days (today--current-files-date) arg)
                   (org-read-date))))
    (today--move-subtree-action date)))

(defcustom today--unfinished-task-regexp "^\\* TODO "
  "The regexp used to search for a incomplete tasks.")

(defun today--move-unfinished-to-date-action (source-date destination-date)
  "Move all unfinished tasks to DATEs file."
  ;; `today--move-subtree-action' is a destructive action, so each iteration
  ;; should have fewer found matches occurring than the previous, there's no
  ;; reason to move the search position along, since it would be changing
  ;; anyway, because of cutting the subtree-at-point.
  (today--move-unfinished-checkboxes-to-date destination-date)
  (with-current-buffer (today--buffer-from-date source-date)
    (while (string-match today--unfinished-task-regexp (buffer-string))
      (if (>= (match-beginning 0) 0)
          (progn
            ;; move 1 character into the found line, to make sure we're on the
            ;; correct line, and not on the last character of the previous line.
            (goto-char (+ 1 (match-beginning 0)))
            (today--move-subtree-action destination-date))))))

;;;###autoload
(defun today-move-unfinished-to-tomorrow ()
  "Move all unfinished tasks in the current buffer, to tomorrows
file."
  (interactive)
  (letrec ((current-files-date (today--current-files-date))
           (tomorrows-date (today--date-add-days current-files-date 1)))
    (today--move-unfinished-to-date-action current-files-date tomorrows-date)))

(defun today--dates-earlier-than (date)
  "Get all available dates, of files earlier than DATE."
  (letrec ((file-dates (today--list-of-files))
           (sorting-fn (lambda (s) (string< s date)))
           (earlier-dates (-filter sorting-fn file-dates)))
    earlier-dates))

;;;###autoload
(defun today-move-unfinished-from-previous ()
  "Move all unfinished tasks from the previous file, to the
current file."
  (interactive)
  (letrec ((current-files-date (today--current-files-date))
           (earlier-dates (today--dates-earlier-than current-files-date))
           (previous-files-date (car earlier-dates)))
    (if previous-files-date
        (today--move-unfinished-to-date-action previous-files-date current-files-date)
      (message "No previous file found!"))))

;;;###autoload
(defun today-move-unfinished-to-date (arg)
  "Move unfinished tasks from the current file to a new date. If
called with a prefix argument, move to the file n-days relative
to the current file, otherwise prompt for a date using
`org-calendar'."
  (interactive "P")
  (letrec ((date (if arg
                     (today--date-add-days (today--current-files-date) arg)
                   (org-read-date)))
           (next-date (today--date-add-days date 1)))
    (today--move-unfinished-to-date-action date next-date)))

(defun today--move-unfinished-checkboxes-to-date (date)
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
          (message "found checkbox task")
          ;; go to the heading containing the found checkboxes
          (goto-char (match-beginning 0))
          (outline-previous-heading)

          ;; copy the entire subtree to the new file, then remove checked
          ;; checkboxes
          (org-copy-subtree)
          (with-current-buffer (today--buffer-from-date date)
            ;; insert at the end of the file
            (end-of-buffer)
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
  (today--move-unfinished-checkboxes-to-date
   (today--date-add-days (today--current-files-date) 1)))

;;;###autoload
(defun today-move-unfinished-checkboxes-to-date ()
  "Move unfinished checkboxes to DATEs file, creating the
containing task/file if it does not exist."
  (interactive)
  (let ((date (org-read-date)))
    (today--move-unfinished-checkboxes-to-date
     (today--date-add-days date 1))))

(require 'hydra)
;; This hydra will exit on one-off commands, such as `today-list', or
;; `today-goto-date', but will persist when using capture or movement commands.
(defhydra today-hydra (:foreign-keys run)
  "
^Capture^                   ^Move^                          ^Actions^
^^^^^^^^--------------------------------------------------------------------------------------
_r_: capture read task      _m_: move to tomorrow             _t_: go to todays file
_w_: capture write task     _d_: move to date                 _l_: list all date files
_c_: capture with prompt    _u_: move TODOs to tomorrow       _g_: go to date from `org-calendar'
^ ^                         _U_: move old TODOs to this file  ^ ^
"
  ("r" (lambda () (interactive) (today-capture-link-with-task 'read)))
  ("w" (lambda () (interactive) (today-capture-link-with-task 'watch)))
  ("c" #'today-capture-prompt)

  ("m" #'today-move-to-tomorrow)
  ("d" #'today-move-to-date)
  ("u" #'today-move-unfinished-to-tomorrow)
  ("U" #'today-move-unfinished-from-previous)

  ("t" #'today :exit t)
  ("l" #'today-list :exit t)
  ("g" #'today-goto-date :exit t)

  ("q" nil "quit"))

(provide 'today)

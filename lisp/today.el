;;; -*- lexical-binding: t -*-
;;; today.el --- simple daily planning

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: fullscreen
;; Package-Version: 20180528
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
  "Todays date."
  (format-time-string "%Y-%m-%d"))

(defun today--current-files-date ()
  "Get the date of the current file."
  (f-base (f-no-ext (buffer-file-name))))

(defun today--date-add-days (date days)
  "Return the date DAYS after DATE."
  (letrec ((date-in-seconds (float-time (date-to-time (concat date " 12:00:00 EST"))))
           (seconds-in-a-day (* 60 60 24))
           (total-seconds-in-days (* days seconds-in-a-day))
           (next-date-in-seconds (+ date-in-seconds total-seconds-in-days))
           (next-date (format-time-string "%Y-%m-%d" next-date-in-seconds)))
    next-date))

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
  "Get the buffer for DATEs file, creates one if it does not
exist."
  (let ((file (today--file-from-date date)))
    (today--create-planning-file file)
    (find-file-noselect file)))

(defun today--visit-date-file (date)
  "Jump to the file for DATE, create it if it does not exist."
  (switch-to-buffer (today--buffer-from-date date)))

(defun today ()
  "Open todays file, create it if it does not exist."
  (interactive)
  (today--visit-date-file (today--todays-date)))

(defun today--capture (date task entry)
  "Captures an ENTRY with TASK, into the file for DATE."
  (save-excursion
    (with-current-buffer (today--buffer-from-date date)
      (end-of-buffer)
      (newline)
      (insert "* TODO " task " " entry))))

(defun today-capture (task entry)
  "Capture ENTRY with TASK into todays file."
  (today--capture (today--todays-date) task entry))

(defvar today--user-agent-string "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36"
  "User agent used when fetching the website title of a link.")

(defun today--get-title-from-link (link)
  "Try to retrieve the website title from a link, requires
`curl', `grep', and `recode'."
  (letrec ((-silence-output " -so - ")
           (-follow-redirects "-L")
           (-use-user-agent "'-A User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36'")
           (grep-title "grep -iPo '(?<=<title>)(.*)(?=</title>)'")
           (recode-to-utf8 "recode html..utf8")
           (curl-command (concat "curl" " " -follow-redirects " " -use-user-agent " '" link "'" -silence-output " | " grep-title " | " recode-to-utf8)))
    (s-trim (shell-command-to-string curl-command))))

(defun today--to-org-link (link title)
  "Convert a link and its title into an `org-link' format."
  (format "[[%s][%s]]" link title))

(defun today--link-to-org-link (link)
  "Try to get the website title of LINK, then convert into
`org-link' format."
  (let ((title (get-title-from-link link)))
    (to-org-link link title)))

(defun today-capture-with-task (task)
  "Prompt for ENTRY, then capture with TASK into todays file."
  (letrec ((link (completing-read "link: " '()))
           (org-link (today--link-to-org-link link)))
    (today-capture task org-link)))

(defun today-capture-prompt ()
  "Captures a LINK into todays file, with the selected TASK."
  (interactive)
  (letrec ((task (completing-read "task: " today-capture-tasks))
           (link (completing-read "link: " '()))
           (org-link (today--link-to-org-link link)))
    (today-capture task org-link)))

(defun today-list ()
  "List all files from `today-directory', jump to the one
selected."
  (interactive)
  (letrec ((dates (-map #'f-base (f-directories today-directory)))
           (date (completing-read "Date: " dates)))
    (today--visit-date-file date)))

(defun today-goto-date ()
  "Prompt for date using `org-calendar', then visit the
corresponding file."
  (interactive)
  (letrec ((date (org-read-date)))
    (today--visit-date-file date)))

(defun today--move-subtree-action (date)
  "Move the org subtree at point, to the bottom of
the file corresponding to DATE."
  (org-cut-subtree)
  (save-buffer)
  (message (format "move action got: %s" date))
  (with-current-buffer (today--buffer-from-date date)
    (org-mode)
    (goto-char (point-max))
    (yank)
    (save-buffer)))

(defun today-move-to-tomorrow ()
  "Move the subtree-at-point to the next days file."
  (interactive)
  (letrec ((current-files-date (today--current-files-date))
           (tomorrows-date (today--date-add-days current-files-date 1)))
    (today--move-subtree-action tomorrows-date)))

(defun today-move-to-date (arg)
  "Move the subtree-at-point to a date selected with
`org-calendar', or, if using the prefix argument, move it n-days
relative to the current file."
  (interactive "P")
  (message (format "arg: %S" arg))
  (letrec ((date (if arg
                     (today--date-add-days (today--current-files-date) arg)
                   (org-read-date))))
    (today--move-subtree-action date)))

(defun today--find-unfinished ()
  "Find all unfinished tasks in the current buffer."
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
  "Move all unfinished tasks to DATEs file."
  (while (string-match "^\\* TODO" (buffer-string))
    (message (buffer-string))
    (if (>= (match-beginning 0) 0)
        (progn
          (goto-char (+ 1 (match-beginning 0)))
          (today--move-subtree-action date)))))

(defun today-move-unfinished-to-tomorrow ()
  "Move all unfinished tasks in the current buffer, to tomorrows
file."
  (interactive)
  (letrec ((current-files-date (today--current-files-date))
           (tomorrows-date (today--date-add-days current-files-date 1))
           (tomorrows-file (today--file-from-date tomorrows-date)))
    (today--create-planning-file tomorrows-file)
    (today--move-unfinished-to-date-action tomorrows-date)))

(defun today-move-unfinished-to-date ()
  "Prompt for a date using `org-calendar', then move all
unfinished tasks in the current buffer to the file for that
date."
  (interactive)
  (letrec ((date (org-read-date))
           (next-date (today--date-add-days date 1))
           (next-file (today--file-from-date next-date)))
    (today--create-planning-file next-file)
    (today--move-unfinished-to-date-action next-date)))

(require 'hydra)
(defhydra today-hydra (:exit t)
  "
^Capture^                   ^Move^                     ^Actions^
^^^^^^^^-----------------------------------------------------------------
_r_: capture read task      _m_: move to tomorrow      _t_: go to todays file
_w_: capture write task     _d_: move to date          _l_: list all date files
_c_: capture with prompt    ^ ^                        _g_: go to date from `org-calendar'
"
  ("r" (lambda () (interactive) (today-capture-with-task "read")))
  ("w" (lambda () (interactive) (today-capture-with-task "watch")))
  ("l" #'today-list)
  ("g" #'today-goto-date)
  ("d" #'today-move-to-date)
  ("m" #'today-move-to-tomorrow)
  ("t" #'today)
  ("c" #'today-capture-prompt))

(provide 'today)

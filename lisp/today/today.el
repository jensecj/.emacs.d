;;; -*- lexical-binding: t -*-
;;; today.el --- simple daily planning

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: org, org-mode, planning, today, todo
;; Package-Version: 20180607
;; Version: 0.5
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

(defcustom today-directory
  (concat user-emacs-directory "today-planner")
  "Directory used for planning files.")

(require 'today-fs)
(require 'today-util)
(require 'today-capture)
(require 'today-move)

;;;###autoload
(defun today ()
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

;; This hydra will exit on one-off commands, such as `today-list', or
;; `today-goto-date', but will persist when using capture or movement commands.
(require 'hydra)
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

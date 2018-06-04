;;; -*- lexical-binding: t -*-

(require 'today-fs)
(require 'today-util)

(defcustom today-capture-tasks
  '(read watch)
  "Tasks that can be captured by `today-capture' functions")

(defun today-capture--read-link-handler (content)
  "Handler for READ task. Expects CONTENT to be a link to some
website. Will try to extract the number of lines on the website,
and add to the front of the entry. Will also try to extract the
title of the website, and convert the link into an `org-mode'
link, using the title."
  (let ((lines (today-util-get-website-lines-from-link content))
        (org-link (today-util-link-to-org-link content)))
    (format "read (%s lines) %s" lines org-link)))

(defun today-capture--watch-link-handler (link)
  "Handler for the WATCH task. Expects CONTENT to be a link to a
website, will try to extract the title of the website, and create
an `org-mode' link using that title."
  (letrec ((title (today-util-get-website-title-from-link link))
           (entry (today-util-to-org-link link title)))
    (format "watch %s" entry)))

(defvar today-capture-handlers-alist
  '((read . today-capture--read-link-handler)
    (watch . today-capture--watch-link-handler))
  "List of capture tasks and their associated handlers. A handler
  recieves the capture content as a parameter.")

(defun today-capture--apply-handler (task entry)
  "If a handler exists for TASK, then return the result of
applying handler on ENTRY, otherwise return ENTRY."
  (let ((handler (assoc task today-capture-handlers-alist)))
    (if handler
        (funcall (cdr handler) entry)
      entry)))

;;;###autoload
(defun today-capture-to-date (date task entry)
  "Captures an ENTRY with TASK, into the file for DATE."
  (let ((content (today-capture--apply-handler task entry)))
    (with-current-buffer (today-fs-buffer-from-date date)
      (end-of-buffer)
      (newline)
      (insert "* TODO " content))))

;;;###autoload
(defun today-capture (task entry)
  "Capture ENTRY with TASK into todays file."
  (today-capture-to-date (today-util-todays-date) task entry))

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

;;;###autoload
(defun today-capture-elfeed-at-point ()
  "Captures a TASK from selected elfeed entry."
  (interactive)
  (letrec ((entry (car (elfeed-search-selected)))
           (link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (org-link (today-util-to-org-link link title)))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (today-capture 'elfeed org-link)
    (next-line)))

(provide 'today-capture)

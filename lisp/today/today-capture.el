;;; -*- lexical-binding: t -*-

(require 'today-fs)
(require 'today-util)

(defcustom today-capture-tasks
  '(read watch)
  "Tasks that can be captured by `today-capture' functions")

(defun today-capture--read-link-handler (link)
  "Handler for READ task. Expects CONTENT to be a link to some
website. Will try to extract the number of lines on the website,
and add to the front of the entry. Will also try to extract the
title of the website, and convert the link into an `org-mode'
link, using the title."
  (letrec ((lines (today-util-get-website-lines-from-link link))
           (org-link (today-util-link-to-org-link link)))
    (format "(%s lines) %s" lines org-link)))

(defun today-capture--watch-link-handler (link)
  "Handler for the WATCH task. Expects the LINK to be a source
compatible with `youtube-dl'.will try to extract the title of the
link, and create an `org-mode' link using that title, will also
extract the duration of the video."
  (letrec ((title (today-util-get-website-title-from-link link))
           (title (replace-regexp-in-string " - YouTube$" "" title))
           (duration (today-util-get-youtube-duration-from-link link))
           (org-link (org-make-link-string link title))
           (entry (format "(%s) %s" duration org-link)))
    (format "%s" entry)))

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
(defun today-capture-to-file-async (task entry)
  "Captures an ENTRY with TASK, into the file for DATE, asynchronously."
  (async-start
   (today-util-async-lambda () (task entry)
     (today-capture--apply-handler task entry))
   (today-util-async-lambda (content) ()
     (with-current-buffer (find-file-noselect (concat today-directory today-file))
       (save-excursion
         (goto-char (point-max))
         (insert "* TODO " content)
         (save-buffer))))))

;;;###autoload
(defun today-capture (task entry)
  "Capture ENTRY with TASK into todays file."
  (today-capture-to-file-async task entry))

(defun today-capture-link-with-task-from-clipboard (task)
  "Capture ENTRY with TASK into todays file."
  (let* ((entry (substring-no-properties
                 (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
    (message "capturing from clipboard: %s" entry)
    (today-capture-to-file-async task entry)))

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
           (org-link (org-make-link-string link title)))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (today-capture 'elfeed org-link)
    (next-line)))

(provide 'today-capture)
